package com.rdio.thor

import java.awt.{Font, FontMetrics, AlphaComposite, Color, Graphics2D, RenderingHints, LinearGradientPaint, MultipleGradientPaint}
import java.awt.geom.{RoundRectangle2D, AffineTransform, Point2D, Rectangle2D}
import java.awt.image.BufferedImage

import com.sksamuel.scrimage.{BufferedOpFilter, Filter, Image, ScaleMethod}

import thirdparty.jhlabs.image.{BoxBlurFilter => BoxBlurFilterOp, ImageUtils}

/** Fast box blur (wrapper around jhlabs filter not exposed by scrimage). */
class BoxBlurFilter(hRadius: Float, vRadius: Float, iterations: Int) extends BufferedOpFilter {
  val op = new BoxBlurFilterOp(hRadius, vRadius, iterations)
}

object BoxBlurFilter {
  def apply(hRadius: Float = 2, vRadius: Float = 2, iterations: Int = 2): BoxBlurFilter =
    new BoxBlurFilter(hRadius, vRadius, iterations)
}

/** Adds a linear gradient to the image */
class LinearGradientFilter(angle: Float, colors: Array[Color], stops: Array[Float]) extends Filter {
  def apply(image: Image) {
    val theta = math.toRadians(angle)

    val startX = 0
    val startY = 0
    val endX = 0
    val endY = image.height

    val cycle = MultipleGradientPaint.CycleMethod.NO_CYCLE
    // val transform = AffineTransform.getRotateInstance(theta)
    val start = new Point2D.Float(startX, startY)
    val end = new Point2D.Float(endX, endY)
    val linearGradient = new LinearGradientPaint(start, end, stops, colors)

    val g2 = image.awt.getGraphics.asInstanceOf[Graphics2D]
    g2.setPaint(linearGradient)
    g2.fillRect(0, 0, image.width, image.height)
    g2.dispose()
  }
}

object LinearGradientFilter {
  def apply(angle: Float, colors: Array[Color], stops: Array[Float]): Filter =
    new LinearGradientFilter(angle, colors, stops)
}

/** Draws the string of a given font in the center of the image */
class TextFilter(text: String, font: Font, color: Color) extends Filter {
  def apply(image: Image) {
    val g2 = image.awt.getGraphics.asInstanceOf[Graphics2D]
    g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g2.setFont(font)
    g2.setColor(color)

    val metrics: FontMetrics = g2.getFontMetrics
    val bounds = metrics.getStringBounds(text, g2)

    val width = bounds.getWidth.toInt

    val x: Int = (image.width - width) / 2

    // Subtract descent because its rendered from baseline while we want to
    // center from ascent height.

    // We use "height - descent" instead of "leading + ascent" due to float
    // rounding errors.
    val y: Int = (image.height + bounds.getHeight.toInt - metrics.getDescent) / 2

    g2.drawString(text, x, y)
  }
}

object TextFilter {
  def apply(text: String, font: Font, color: Color): Filter = new TextFilter(text, font, color)
}

/** Blends between two images using a mask */
class MaskFilter(overlay: Image, mask: Image) extends Filter {
  def apply(image: Image) {
    val imageRaster = image.awt.getRaster()
    val overlayRaster = overlay.awt.getRaster()
    val maskRaster = mask.awt.getRaster()
    ImageUtils.composeThroughMask(overlayRaster, imageRaster, maskRaster)
  }
}

object MaskFilter {
  def apply(overlay: Image, mask: Image): Filter =
    new MaskFilter(overlay, mask)
}

/** Rounds the corners of the image (uses compositing for AA). */
class RoundCornersFilter(radius: Int) extends Filter {
  def apply(image: Image) {
    val working = image.empty.awt
    val g2 = working.getGraphics.asInstanceOf[Graphics2D]
    g2.setComposite(AlphaComposite.Src)
    g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g2.setColor(Color.WHITE)
    g2.fillRoundRect(0, 0, image.width, image.height, radius, radius)
    g2.setComposite(AlphaComposite.SrcIn)
    g2.drawImage(image.awt, 0, 0, null)
    g2.dispose()

    val imageG2 = image.awt.getGraphics.asInstanceOf[Graphics2D]
    imageG2.setBackground(new Color(255, 255, 255, 0))
    imageG2.clearRect(0, 0, image.width, image.height)
    imageG2.drawImage(working, 0, 0, null)
    imageG2.dispose()
  }
}

object RoundCornersFilter {
  def apply(radius: Int): Filter = new RoundCornersFilter(radius)
}

/** Draws an image over the current image. */
class OverlayFilter(overlay: Image) extends Filter {
  def apply(image: Image) {
    image._draw(overlay.awt)
  }
}

object OverlayFilter {
  def apply(overlay: Image): Filter =
    new OverlayFilter(overlay: Image)
}

/** Combines an array of images into a single image. */
class GridFilter(images: Array[Image]) extends Filter {
  def apply(working: Image) {
    val count: Int = images.length

    // Look for a square value of length (1, 4, 9)
    val nextSqr: Int = math.pow(math.ceil(math.sqrt(count.toDouble)), 2).toInt

    // Pad the array until its length is an evenly square rootable number
    val paddedImages: Array[Image] = (for (i <- 0 until nextSqr) yield images(i % count)).toArray

    // The number of images per row or column is the sqrt of our count (which is a square)
    val perRow: Int = math.sqrt(nextSqr).toInt

    // Find a size that fits our grid
    val sourceWidth: Int = math.ceil(working.width.toFloat / perRow.toFloat).toInt
    val sourceHeight: Int = math.ceil(working.height.toFloat / perRow.toFloat).toInt
    val sourceSize: Int = math.max(sourceWidth, sourceHeight)

    val g2 = working.awt.getGraphics.asInstanceOf[Graphics2D]
    g2.clearRect(0, 0, working.width, working.height)

    // Recursively draw each grid item, passing the remaining grid items on to the next call
    def drawImages(images: Array[Image], xOffset: Int = 0, yOffset: Int = 0) {
      if (!images.isEmpty) {
        // Take the first image and scale it
        val rescaled = images.head.scaleTo(sourceSize, sourceSize, ScaleMethod.Bicubic)

        // Draw the image onto our final grid canvas
        g2.drawImage(rescaled.awt, xOffset, yOffset, sourceSize, sourceSize, null)

        // Calculate the next images coordinates
        val nextXOffset = (xOffset + sourceSize) % working.width

        // If the xOffset wrapped to 0, we're on a new row (increment yOffset)
        val nextYOffset = if (nextXOffset < sourceSize) { // We've wrapped, next row
          (yOffset + sourceSize) % working.height
        } else {
          yOffset
        }

        // Recur
        drawImages(images.tail, nextXOffset, nextYOffset)
      }
    }

    drawImages(paddedImages)

    g2.dispose()
  }
}

object GridFilter {
  def apply(images: Array[Image]): Filter = new GridFilter(images)
}
