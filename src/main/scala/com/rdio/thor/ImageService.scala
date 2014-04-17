package com.rdio.thor

import java.awt.Color
import java.io.{File, FileInputStream}
import java.net.InetSocketAddress
import java.util.{Calendar, Date}

import scala.collection.mutable.ArrayBuffer

import com.sksamuel.scrimage.{Format, Image, ImageTools, ScaleMethod}
import com.sksamuel.scrimage.io.{ImageWriter, JpegWriter, PngWriter}
import com.sksamuel.scrimage.filter.{ColorizeFilter, BlurFilter}

import com.twitter.conversions.time._
import com.twitter.finagle.Service
import com.twitter.finagle.builder.ClientBuilder
import com.twitter.finagle.http.{Http, Status, RichHttp, Request, Response, Message}
import com.twitter.util.{Await, Future}

import com.typesafe.config.Config

import org.jboss.netty.handler.codec.http._
import org.jboss.netty.buffer.ChannelBuffers

/** ImageService serves images optionally filtered and blended. */
class ImageService(conf: Config) extends BaseImageService(conf) {

  protected def parserFactory(width: Int, height: Int) = new LayerParser(width, height)

  def tryGetImage(pathOrImage: ImageNode, imageMap: Map[String, Image], completedLayers: Array[Image], width: Int, height: Int): Option[Image] = {
    pathOrImage match {
      case IndexNode(index) if index < completedLayers.length => Some(completedLayers(index))
      case PathNode(path) if imageMap.contains(path) => imageMap.get(path)
      case EmptyNode() => Some(Image.filled(width, height, new Color(0, 0, 0, 0)))
      case PreviousNode() if completedLayers.nonEmpty => Some(completedLayers.last)
    }
  }

  def applyFilter(image: Image, filter: FilterNode, imageMap: Map[String, Image], completedLayers: Array[Image], width: Int, height: Int): Option[Image] = {
    filter match {

      case LinearGradientNode(degrees, colors, stops) =>
        Some(image.filter(LinearGradientFilter(degrees, colors.toArray, stops.toArray)))

      case BlurNode() => Some(image.filter(BlurFilter))

      case BoxBlurNode(hRadius, vRadius) => {
        val originalWidth = image.width
        val originalHeight = image.height
        val downsampleFactor = 2
        val downsampling = 1.0f / downsampleFactor
        val downsampledHRadius: Int = math.round(hRadius * downsampling)
        val downsampledVRadius: Int = math.round(vRadius * downsampling)

        Some {
          image.scale(downsampling).filter(BoxBlurFilter(downsampledHRadius, downsampledVRadius))
            .trim(1, 1, 1, 1) // Remove bleeded edges
            .scaleTo(originalWidth, originalHeight, ScaleMethod.Bicubic) // Scale up a bit to account for trim
        }
      }

      case ColorizeNode(color) => Some(image.filter(ColorizeFilter(color)))

      case ZoomNode(percentage) => {
        val originalWidth = image.width
        val originalHeight = image.height
        Some {
          image.scale(1.0f + percentage, ScaleMethod.Bicubic)
            .resizeTo(originalWidth, originalHeight)
        }
      }

      case ScaleNode(percentage) => Some(image.scale(percentage, ScaleMethod.Bicubic))

      case ScaleToNode(width, height) => Some(image.scaleTo(width, height, ScaleMethod.Bicubic))

      case ConstrainNode(constraints) => None

      case CompositeNode(path, composites) => None

      case PadNode(padding) => Some(image.pad(padding, new Color(0, 0, 0, 0)))

      case GridNode(paths) => {
        val images: List[Image] = paths.flatMap {
          path => tryGetImage(path, imageMap, completedLayers, width, height)
        }
        if (images.nonEmpty) {
          if (images.length > 1) {
            Some(image.filter(GridFilter(images.toArray)))
          } else {
            Some(images.head)
          }
        } else {
          log.error(s"Failed to apply grid")
          None
        }
      }

      case RoundCornersNode(radius) => Some(image.filter(RoundCornersFilter(radius)))

      case CoverNode(width, height) => Some(image.cover(width, height, ScaleMethod.Bicubic))

      case OverlayNode(overlay) => {
        tryGetImage(overlay, imageMap, completedLayers, width, height) match {
          case Some(overlayImage) => {
            Some(image.filter(OverlayFilter(overlayImage.scaleTo(width, height, ScaleMethod.Bicubic))))
          }
          case _ => {
            log.error(s"Failed to apply overlay")
            None
          }
        }
      }

      case MaskNode(overlay, mask) => {
        val overlayOption = tryGetImage(overlay, imageMap, completedLayers, width, height)
        val maskOption = tryGetImage(mask, imageMap, completedLayers, width, height)
        (overlayOption, maskOption) match {
          case (Some(overlayImage), Some(maskImage)) => {
            Some {
              image.filter {
                // We resize the overlay and mask since the filter requires that they be the same size
                MaskFilter(overlayImage.scaleTo(image.width, image.height, ScaleMethod.Bicubic),
                  maskImage.scaleTo(image.width, image.height, ScaleMethod.Bicubic))
              }
            }
          }
          case _ => {
            log.error(s"Failed to apply mask")
            None
          }
        }
      }

      case _: NoopNode => Some(image)
    }
  }

  def applyLayerFilters(imageMap: Map[String, Image], layers: List[LayerNode], width: Int, height: Int): Option[Image] = {
    // Apply each layer in order
    val completedLayers = ArrayBuffer.empty[Image]
    layers foreach {
      case LayerNode(path: ImageNode, filter: FilterNode) => {
        tryGetImage(path, imageMap, completedLayers.toArray, width, height) match {
          case Some(baseImage) => {
            applyFilter(baseImage, filter, imageMap, completedLayers.toArray, width, height) match {
              case Some(filteredImage) => completedLayers += filteredImage
              case None => {
                log.error(s"Failed to apply layer filter: $path $filter")
                None
              }
            }
          }
          case None => {
            log.error(s"Failed to get layer source: $path")
            None
          }
        }
      }
    }
    completedLayers.lastOption
  }

  def apply(req: Request): Future[Response] = {
    log.info(s"Received request: ${req.params}")

    req.params.get("l") match {
      case Some(layers) => {
        // Restrict dimensions to the range 200-1200
        val width: Int = math.min(math.max(req.params.getIntOrElse("w", 200), 200), 1200)
        val height: Int = math.min(math.max(req.params.getIntOrElse("h", 200), 200), 1200)

        // Restrict compression to the range 0-100
        val compression: Int = math.min(math.max(req.params.getIntOrElse("c", 98), 0), 100)

        val parser = parserFactory(width, height)

        // Parse the layers and attempt to handle each layer
        parser.parseAll(parser.layers, layers) match {
          case parser.Success(layers, _) => {
            // Extract all paths
            val paths = layers flatMap {
              case LayerNode(path, GridNode(paths)) => path +: paths
              case LayerNode(path, MaskNode(overlay, mask)) => path +: List(overlay, mask)
              case LayerNode(path, OverlayNode(overlay)) => path +: List(overlay)
              case LayerNode(path, _: FilterNode) => List(path)
            } flatMap {
              case PathNode(path) => List(path)
              case _ => List()
            }

            // Fetch images by paths
            requestImages(paths.toArray) map {
              potentialImages => {
                // Build a map of paths to images (removing empty paths)
                val imageMap = (paths zip potentialImages).toMap.flatMap {
                  case (path, Some(image)) => List((path, image))
                  case (_, None) => List()
                }

                // Apply any filters to each image and return the final image
                applyLayerFilters(imageMap, layers, width, height) match {
                  case Some(image) => {
                    // Look for occurences of rounded corners
                    val containsTranslucentFilters: Boolean = layers.exists {
                      case LayerNode(_, filter) => filter match {
                        case _: RoundCornersNode => true
                        case _ => false
                      }
                    }

                    // Switch to PNG if we used rounded corners
                    val format: Format[ImageWriter] = if (containsTranslucentFilters) {
                      Format.PNG.asInstanceOf[Format[ImageWriter]]
                    } else {
                      Format.JPEG.asInstanceOf[Format[ImageWriter]]
                    }

                    // Apply final resize and build response
                    buildResponse(req, image.scaleTo(width, height, ScaleMethod.Bicubic), format, compression)
                  }
                  case None => {
                    Response(HttpVersion.HTTP_1_1, HttpResponseStatus.NOT_FOUND)
                  }
                }
              }
            }
          }
          case unsuccess: parser.NoSuccess => {
            log.error(s"Failed to parse layers: $layers - ${unsuccess.msg}")
            Future.value(Response(HttpVersion.HTTP_1_1, HttpResponseStatus.NOT_FOUND))
          }
        }
      }
      case None => {
        log.error(s"No layers found in request: ${req.uri}")
        Future.value(Response(HttpVersion.HTTP_1_1, HttpResponseStatus.NOT_FOUND))
      }
    }
  }
}
