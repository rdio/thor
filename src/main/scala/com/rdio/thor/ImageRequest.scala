package com.rdio.thor

import java.awt.{Color, Font, GraphicsEnvironment}
import java.io.{File, FileInputStream}
import java.net.InetSocketAddress
import java.util.{Calendar, Date}

import scala.collection.mutable.ArrayBuffer

import com.rdio.thor.extensions._

import com.sksamuel.scrimage.{Format, Image, ImageTools, ScaleMethod}
import com.sksamuel.scrimage.io.{ImageWriter, JpegWriter, PngWriter}
import com.sksamuel.scrimage.filter.{ColorizeFilter, BlurFilter}

import com.twitter.conversions.time._
import com.twitter.finagle.Service
import com.twitter.finagle.builder.ClientBuilder
import com.twitter.finagle.http.{Http, Status, RichHttp, Request, Response, Message}
import com.twitter.logging.Logger
import com.twitter.util.{Await, Future}
import com.typesafe.config.Config

import org.jboss.netty.handler.codec.http._
import org.jboss.netty.buffer.ChannelBuffers

class ImageRequest(
  layers: List[LayerNode], // Layers constructed so far
  fetchedImages: Map[String, Image], // Images fetched so far
  completedLayers: Array[Image], // Layers with filters applied
  requestWidth: Option[Int],
  requestHeight: Option[Int]
) {

  protected lazy val log = Logger.get(this.getClass)

  // Try to determine the next layers dimensions from the
  // provided image dimensions and request dimensions
  def getDimensions(image: Option[Image]): Tuple2[Int, Int] = {
    (requestWidth, requestHeight) match {
      // Both provided
      case (Some(width), Some(height)) => (width, height)

      // Width provided
      case (Some(width), None) => {
        image match {

          // Image provided
          case Some(image) => (width, (width.toFloat / image.aspectRatio).toInt)

          // No image to infer aspect ratio
          case None => (width, width)
        }
      }

      // Height provided
      case (None, Some(height)) => {
        image match {

          // Image provided
          case Some(image) => ((image.aspectRatio * height.toFloat).toInt, height)

          // No image to infer aspect ratio
          case None => (height, height)
        }
      }

      // None provided
      case (None, None) => {
        image match {

          // Image provided
          case Some(image) => (image.width, image.height)

          // No image to infer aspect ratio
          case None => (200, 200)
        }
      }
    }
  }

  def getFilledImage(color: Color = new Color(0, 0, 0, 0)): Image = {
    // The size of the empty layer is dependent on request dimensions
    // and the previous layer dimensions
    val (width, height) = getDimensions(completedLayers.lastOption)
    Image.filled(width, height, color)
  }

  def getImage(source: ImageNode): Option[Image] = {
    source match {
      case IndexNode(index) if index < completedLayers.length => Some(completedLayers(index))
      case PathNode(path) if fetchedImages.contains(path) => fetchedImages.get(path)
      case PreviousNode() if completedLayers.nonEmpty => Some(completedLayers.last)
      case EmptyNode() => Some(getFilledImage())
      case _ => None
    }
  }

  def applyFilter(image: Image, filter: FilterNode): Option[Image] = {
    filter match {

      case LinearGradientNode(degrees, colors, stops) =>
        Some(image.filter(LinearGradientFilter(degrees, colors.toArray, stops.toArray)))

      case BlurNode() => Some(image.filter(BlurFilter))

      case BoxBlurNode(hRadius, vRadius) => {
        val originalWidth = image.width
        val originalHeight = image.height
        val downsampleFactor = 4
        val downsampling = 1.0f / downsampleFactor
        val downsampledHRadius: Int = math.round(hRadius * downsampling)
        val downsampledVRadius: Int = math.round(vRadius * downsampling)

        Some {
          image.scale(downsampling).filter(BoxBlurFilter(downsampledHRadius, downsampledVRadius))
            .trim(1, 1, 1, 1) // Remove bleeded edges
            .scaleTo(originalWidth, originalHeight, ScaleMethod.Bicubic) // Scale up a bit to account for trim
        }
      }

      case BoxBlurPercentNode(hPercent, vPercent) => {
        val originalWidth = image.width
        val originalHeight = image.height
        val hRadius = (hPercent * originalWidth.toFloat).toInt
        val vRadius = (vPercent * originalHeight.toFloat).toInt
        val downsampleFactor = 4
        val downsampling = 1.0f / downsampleFactor
        val downsampledHRadius: Int = math.round(hRadius * downsampling)
        val downsampledVRadius: Int = math.round(vRadius * downsampling)

        Some {
          image.scale(downsampling).filter(BoxBlurFilter(downsampledHRadius, downsampledVRadius))
            .trim(1, 1, 1, 1) // Remove bleeded edges
            .scaleTo(originalWidth, originalHeight, ScaleMethod.Bicubic) // Scale up a bit to account for trim
        }
      }

      case TextNode(text, font, color) => {
        font match {
          case FontNode(family, size, style) => {
            val ge: GraphicsEnvironment = GraphicsEnvironment.getLocalGraphicsEnvironment()
            val fontFamilies: Array[String] = ge.getAvailableFontFamilyNames()
            try {
              val font: Font = if (fontFamilies.contains(family)) {
                new Font(family, style, size)
              } else {
                val resourceStream = getClass.getResourceAsStream(s"/fonts/$family.ttf")
                val font: Font = Font.createFont(Font.TRUETYPE_FONT, resourceStream)
                font.deriveFont(style, size)
              }
              Some(image.filter(TextFilter(text, font, color)))
            } catch {
              case _: Exception => None
            }
          }
        }
      }

      case TextPercentNode(text, font, color) => {
        font match {
          case FontPercentNode(family, percentage, style) => {
            val ge: GraphicsEnvironment = GraphicsEnvironment.getLocalGraphicsEnvironment()
            val fontFamilies: Array[String] = ge.getAvailableFontFamilyNames()
            val size: Int = (percentage * math.max(image.width, image.height).toFloat).toInt
            try {
              val font: Font = if (fontFamilies.contains(family)) {
                new Font(family, style, size)
              } else {
                val resourceStream = getClass.getResourceAsStream(s"/fonts/$family.ttf")
                val font: Font = Font.createFont(Font.TRUETYPE_FONT, resourceStream)
                font.deriveFont(style, size)
              }
              Some(image.filter(TextFilter(text, font, color)))
            } catch {
              case _: Exception => None
            }
          }
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

      case PadNode(padding) => Some(image.pad(padding, new Color(0, 0, 0, 0)))

      case PadPercentNode(percent) => {
        val padding = (percent * math.max(image.width, image.height).toFloat).toInt
        Some(image.pad(padding, new Color(0, 0, 0, 0)))
      }

      case GridNode(paths) => {
        val images: List[Image] = paths.flatMap {
          path => getImage(path)
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

      case RoundCornersPercentNode(percent) => {
        val radius = (percent * math.max(image.width, image.height).toFloat).toInt
        Some(image.filter(RoundCornersFilter(radius)))
      }

      case CoverNode(width, height) => Some(image.cover(width, height, ScaleMethod.Bicubic))

      case OverlayNode(overlay) => {
        getImage(overlay) match {
          case Some(overlayImage) => {
            Some(image.filter(OverlayFilter(overlayImage.condScaleTo(image.width, image.height))))
          }
          case _ => {
            log.error(s"Failed to apply overlay: $overlay failed to load")
            None
          }
        }
      }

      case MaskNode(overlay, mask) => {
        val overlayOption = getImage(overlay)
        val maskOption = getImage(mask)
        (overlayOption, maskOption) match {
          case (Some(overlayImage), Some(maskImage)) => {
            Some {
              image.filter {
                // We resize the overlay and mask since the filter requires that they be the same size
                MaskFilter(overlayImage.condScaleTo(image.width, image.height),
                  maskImage.condScaleTo(image.width, image.height))
              }
            }
          }
          case _ => {
            log.error(s"Failed to apply mask: $overlay or $mask failed to load")
            None
          }
        }
      }

      case _: NoopNode => Some(image)
    }
  }

  // Apply any filters to each image and return the final image
  def apply(): Option[Image] = {
    // Take the next layer
    layers.headOption match {
      case Some(layer) => {
        layer match {
          case LayerNode(path: ImageNode, filter: FilterNode) => {
            getImage(path) match {
              case Some(image) => {
                // Determine current width/height
                val (width, height) = getDimensions(Some(image))

                // Resize the image before applying filters to do less work
                applyFilter(image.condScaleTo(width, height), filter) match {
                  case Some(filteredImage) => {
                    // Apply the next request with remaining layers and new filtered image
                    val request = new ImageRequest(layers.tail, fetchedImages, completedLayers :+ filteredImage, Some(width), Some(height))
                    request()
                  }
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
      }
      case None => {
        // We've run out of layers — apply final resize
        completedLayers.lastOption match {
          case Some(image) => {
            // Determine current width/height
            val (width, height) = getDimensions(Some(image))
            Some(image.condScaleTo(width, height))
          }
          case None => None
        }
      }
    }
  }
}
