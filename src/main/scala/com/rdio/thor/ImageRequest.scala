package com.rdio.thor

import java.awt.{Color, Font, GraphicsEnvironment}
import java.io.{File, FileInputStream}
import java.util.{Calendar, Date}

import scala.collection.mutable.ArrayBuffer

import com.rdio.thor.extensions._

import com.sksamuel.scrimage.{Format, Image, ScaleMethod}
import com.sksamuel.scrimage.filter.{ColorizeFilter, BlurFilter}

import com.twitter.conversions.time._
import com.twitter.finagle.Service
import com.twitter.finagle.builder.ClientBuilder
import com.twitter.finagle.http.{Http, Status, RichHttp, Request, Response, Message}
import com.twitter.finagle.stats.ServerStatsReceiver
import com.twitter.logging.Logger
import com.twitter.util.{Await, Future}
import com.typesafe.config.Config

import org.jboss.netty.handler.codec.http._
import org.jboss.netty.buffer.ChannelBuffers

// Encapsulates state involved in processing an image request
class ImageRequest(
  layers: List[LayerNode],
  fetchedImages: Map[String, Image], // Images fetched
  requestWidth: Option[Int],
  requestHeight: Option[Int]
) {

  lazy val log = Logger.get(this.getClass)

  // Try to determine the next layers dimensions from the
  // provided image dimensions and request dimensions
  def getDimensions(image: Option[Image], width: Option[Int], height: Option[Int]): Tuple2[Int, Int] = {
    (width, height) match {
      // Both provided
      case (Some(w), Some(h)) => (w, h)

      // Width provided
      case (Some(w), None) => {
        image match {

          // Image provided
          case Some(image) => (w, (w.toFloat / image.aspectRatio).toInt)

          // No image to infer aspect ratio
          case None => (w, w)
        }
      }

      // Height provided
      case (None, Some(h)) => {
        image match {

          // Image provided
          case Some(image) => ((image.aspectRatio * h.toFloat).toInt, h)

          // No image to infer aspect ratio
          case None => (h, h)
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

  def getImage(source: ImageNode, completedLayers: Array[Image]): Option[Image] = {
    source match {
      case IndexNode(index) if index < completedLayers.length => Some(completedLayers(index))
      case UrlNode(url) if fetchedImages.contains(url) => fetchedImages.get(url)
      case PreviousNode() if completedLayers.nonEmpty => Some(completedLayers.last)
      case EmptyNode() => {
        Some {
          // The size of the empty layer is dependent on the current dimensions
          // and the previous layer dimensions
          val (w, h) = getDimensions(completedLayers.lastOption, requestWidth, requestHeight)
          Image.filled(w, h, Color.BLACK)
        }
      }
      case _ => None
    }
  }

  def getFont(font: FontNode, image: Image): Option[Font] = {
    val ge: GraphicsEnvironment = GraphicsEnvironment.getLocalGraphicsEnvironment()
    val fontFamilies: Array[String] = ge.getAvailableFontFamilyNames()
    val (family, size, style) = font match {
      case FontPixelsNode(family, size, style) => {
        (family, size, style)
      }
      case FontPercentNode(family, percentage, style) => {
        val size: Int = (percentage * math.max(image.width, image.height).toFloat).toInt
        (family, size, style)
      }
    }
    try {
      val font: Font = if (fontFamilies.contains(family)) {
        new Font(family, style, size)
      } else {
        val resourceStream = getClass.getResourceAsStream(s"/fonts/$family.ttf")
        val font: Font = Font.createFont(Font.TRUETYPE_FONT, resourceStream)
        resourceStream.close()
        font.deriveFont(style, size)
      }
      Some(font)
    } catch {
      case _: Exception => {
        log.error(s"Could not find or load font $family")
        None
      }
    }
  }

  def applyFilter(image: Image, filter: FilterNode, completedLayers: Array[Image]): Option[Image] = {
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
        getFont(font, image).map(font => image.filter(TextFilter(text, font, color)))
      }

      case TextPositionedNode(text, font, color, pos, hAlign, vAlign, fit) => {
        getFont(font, image).map(font => image.filter(TextFilter(text, font, color, pos, hAlign, vAlign, fit)))
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
          path => getImage(path, completedLayers)
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

      case CoverNode() => {
        val (w, h) = getDimensions(completedLayers.lastOption, requestWidth, requestHeight)
        Some(image.cover(w, h, ScaleMethod.Bicubic))
      }

      case FitNode() => {
        val (w, h) = getDimensions(completedLayers.lastOption, requestWidth, requestHeight)
        Some(image.fit(w, h, new Color(0, 0, 0, 0), ScaleMethod.Bicubic))
      }

      case OverlayNode(overlay) => {
        getImage(overlay, completedLayers) match {
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
        val overlayOption = getImage(overlay, completedLayers)
        val maskOption = getImage(mask, completedLayers)
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

      case FrameNode(thickness, color) =>
        Some(image.filter(FrameFilter(thickness, color)))

      case _: NoopNode => Some(image)
    }
  }

  // Apply any filters to each image and return the final image
  def apply(): Option[Image] = {
    val statsReceiver = ServerStatsReceiver

    layers.foldLeft((Array.empty[Image])) {
      case ((completedLayers), LayerNode(source: ImageNode, filter: FilterNode)) => {
        getImage(source, completedLayers) match {
          case Some(image) => {
            // Resize the image before applying filters to do less work
            statsReceiver.time("filter", filter.toString()) { applyFilter(image, filter, completedLayers) } match {
              case Some(filteredImage) => {
                // Apply the next request with remaining layers and new filtered image
                (completedLayers :+ filteredImage)
              }
              case None => {
                log.error(s"Failed to apply layer filter: $source $filter")
                (completedLayers)
              }
            }
          }
          case None => {
            log.error(s"Failed to get layer source: $source")
            (completedLayers)
          }
        }
      }

    // We've run out of layers — apply final resize
    } match {
      case (completedLayers) => {
        completedLayers.lastOption match {
          case Some(image) => {
            // Determine current width/height
            val (w, h) = getDimensions(Some(image), requestWidth, requestHeight)
            Some(statsReceiver.time("filter", "condscaleto") { image.condScaleTo(w, h) })
          }
          case None => None
        }
      }
    }
  }
}
