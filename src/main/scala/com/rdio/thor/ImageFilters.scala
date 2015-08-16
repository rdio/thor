package com.rdio.thor

import scala.annotation.tailrec

import java.awt.{Font, FontMetrics, AlphaComposite, Color, Graphics2D, RenderingHints, LinearGradientPaint, MultipleGradientPaint, Rectangle}
import java.awt.geom.{RoundRectangle2D, AffineTransform, Point2D, Rectangle2D}
import java.awt.font.{FontRenderContext, GlyphVector}
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

sealed trait HorizontalAlignment
sealed trait VerticalAlignment
case class LeftAlign() extends HorizontalAlignment
case class CenterAlign() extends HorizontalAlignment with VerticalAlignment
case class RightAlign() extends HorizontalAlignment
case class BottomAlign() extends VerticalAlignment
case class TopAlign() extends VerticalAlignment

sealed trait ImagePosition
case class Centered() extends ImagePosition
case class CartesianAbsolute(dx: Int, dy: Int) extends ImagePosition
case class CartesianRelative(percentX: Float, percentY: Float) extends ImagePosition

sealed trait TextWidth
case class WidthFromContent() extends TextWidth
case class WidthFitted(fitTo: Int, maxFontSize: Length) extends TextWidth

sealed trait Length
case class LengthPercentHeight(percent: Float) extends Length
case class LengthPercentWidth(percent: Float) extends Length
case class LengthPercentage(percent: Float) extends Length
case class LengthPixels(pixels: Int) extends Length

object Length {
  def toAbsolutePreferWidth(l: Length, image: Image): Int = l match {
    case LengthPercentHeight(p) => (p * image.height).toInt
    case LengthPercentWidth(p) => (p * image.width).toInt
    case LengthPercentage(p) => (p * image.width).toInt
    case LengthPixels(d) => d
  }

  def toAbsolutePreferHeight(l: Length, image: Image): Int = l match {
    case LengthPercentHeight(p) => (p * image.height).toInt
    case LengthPercentWidth(p) => (p * image.width).toInt
    case LengthPercentage(p) => (p * image.height).toInt
    case LengthPixels(d) => d
  }
}

/** Draws the string of a given font at the specified position. */
class TextFilter(
  text: String, 
  font: Font, 
  color: Color,
  imagePos: List[ImagePosition], // the image positions are additive, so that you could do a relative offset, and then adjust by a few pixels
  horizontalAlignment: HorizontalAlignment,
  verticalAlignment: VerticalAlignment,
  textWidth: TextWidth) extends Filter
{
  def apply(image: Image) {
    val g2 = image.awt.getGraphics.asInstanceOf[Graphics2D]
    g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g2.setColor(color)

    textWidth match {
      case WidthFromContent() => {
        g2.setFont(font)
      }
      case WidthFitted(fitTo, maxFontSize) => {
        val maxFontSizeInPixels = Length.toAbsolutePreferWidth(maxFontSize, image)
        val fontSize = fitTextToWidth(g2, text, fitTo, maxFontSizeInPixels)
        g2.setFont(font.deriveFont(font.getStyle, fontSize))
      }
    }

    val (x,y) = imagePos.map(imagePositionToCoords(image)).reduce((a: (Int,Int), b: (Int,Int)) => (a,b) match {
      case ((x1,y1), (x2, y2)) => (x1+x2, y1+y2)
    })

    // if textX or textY are outside the image bounds, then the text bocomes cropped,
    // but it draws correctly where you'd expect it to. this is the desired behavior,
    // because what if the designers wanted the text to flow in for artistic effect?
    val (textX, textY) = align(g2, text, x, y, horizontalAlignment, verticalAlignment)
    g2.drawString(text, textX, textY)
    g2.dispose()
  }

  def imagePositionToCoords(image: Image)(imagePos: ImagePosition): (Int, Int) = imagePos match {
    case Centered() => (image.width / 2, image.height / 2)
    case CartesianAbsolute(x,y) => (x,y)
    case CartesianRelative(percentX, percentY) => ((image.width * percentX).toInt, (image.height * percentY).toInt)
  }

  def getRectViaFontMetrics(g2: Graphics2D, text: String): Rectangle = {
    val fontMetrics: FontMetrics = g2.getFontMetrics
    fontMetrics.getStringBounds(text, g2).getBounds
  }

  def getRectViaGlyphVector(g2: Graphics2D, text: String): Rectangle = {
    val graphicsFont: Font = g2.getFont
    val renderContext: FontRenderContext = g2.getFontRenderContext
    val glyphVector: GlyphVector = graphicsFont.createGlyphVector(renderContext, text)
    glyphVector.getVisualBounds().getBounds()
  }

  // Lets say you want to add the name of a band to some station image, but you
  // don't want the text to overflow off the image. This function uses binary
  // search to find the largest font size that fits within the given width.
  def fitTextToWidth(g2: Graphics2D, text: String, width: Int, maxFontSize: Int): Int = {
    val minSize = 4
    if (getWidthOfText(g2, text, minSize) > width) {
      // oy...
      return minSize
    }

    if (getWidthOfText(g2, text, maxFontSize) <= width) {
      return maxFontSize
    }

    @tailrec
    def search(lo: Int, hi: Int): Int = {
      val mid = (hi+lo) / 2
      mid match {
        case _ if (lo == mid) =>
          lo

        case _ if (width < getWidthOfText(g2, text, mid)) =>
          search(lo, mid)

        case _ =>
          search(mid, hi)
      }
    }

    search(minSize, maxFontSize)
  }

  def getWidthOfText(g2: Graphics2D, text: String, fontSize: Int): Int = {
    val savedFont = g2.getFont()
    val test = font.deriveFont(font.getStyle, fontSize)
    g2.setFont(test)
    val width = getRectViaFontMetrics(g2, text).width
    g2.setFont(savedFont)

    width
  }

  def align(g2: Graphics2D, text: String, x: Int, y: Int, 
            horizontalAlignment: HorizontalAlignment,
            verticalAlignment: VerticalAlignment): (Int, Int) = {
    val stringBounds = getRectViaFontMetrics(g2, text)
    val visualBounds = getRectViaGlyphVector(g2, text)
    val textX = horizontalAlignment match {
      case LeftAlign() => x
      case CenterAlign() => x - stringBounds.width / 2
      case RightAlign() => x - stringBounds.width
    }
    val textY = verticalAlignment match {
      case TopAlign() => y - visualBounds.y
      case CenterAlign() => y - visualBounds.height / 2 - visualBounds.y
      case BottomAlign() => y - visualBounds.height - visualBounds.y
    }
    return (textX, textY)
  }
}

object TextFilter {
  def apply(text: String, font: Font, color: Color, imagePos: List[ImagePosition], horizontalAlignment: HorizontalAlignment, verticalAlignment: VerticalAlignment, textWidth: TextWidth): Filter =
    new TextFilter(text, font, color, imagePos, horizontalAlignment, verticalAlignment, textWidth)
  def apply(text: String, font: Font, color: Color, imagePos: List[ImagePosition], horizontalAlignment: HorizontalAlignment, verticalAlignment: VerticalAlignment): Filter =
    new TextFilter(text, font, color, imagePos, horizontalAlignment, verticalAlignment, WidthFromContent())
  def apply(text: String, font: Font, color: Color): Filter =
    new TextFilter(text, font, color, List(Centered()), CenterAlign(), CenterAlign(), WidthFromContent())
}

/** Blends between two images using a mask */
class MaskFilter(overlay: Image, mask: Image) extends Filter {
  def apply(image: Image) {
    val imageRaster = image.awt.getRaster()
    val overlayRaster = overlay.awt.getRaster()

    // composeThroughMask looks at the opacity of the maskRaster but we're
    // going to translate that from a grayscale version of the mask where
    // white is opacity 100% and black opacity 0%
    // Copying the image so we don't desaturate the actual mask layer
    val maskCopy = mask.copy
    val maskRaster = maskCopy.awt.getRaster()
    var pixels:Array[Double] = null
    pixels = maskRaster.getPixels(0, 0, maskRaster.getWidth(), maskRaster.getHeight(), pixels)
    for( i <- 0 until pixels.length by 4) {
        // common grayscale equation. See http://www.thewonderoflight.com/articles/definitive-guide-to-bw-conversion/
        val desaturated = pixels(i) * 0.3 + pixels(i + 1) * 0.59 + pixels(i + 2) * 0.11
        pixels.update(i + 3, desaturated)
    }
    maskRaster.setPixels(0, 0, maskRaster.getWidth(), maskRaster.getHeight(), pixels)

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
    val g2 = image.awt.getGraphics.asInstanceOf[Graphics2D]
    g2.drawImage(overlay.awt, 0, 0, null)
    g2.dispose()
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

/** Adds a frame around the edge of the image */
case class FrameFilter(thickness: Length, color: Color) extends Filter {
  def apply(image: Image) {
    val g2 = image.awt.getGraphics.asInstanceOf[Graphics2D]
    val verticalThickness = Length.toAbsolutePreferHeight(thickness, image)
    val horizontalThickness = Length.toAbsolutePreferWidth(thickness, image)

    g2.setColor(color)
    g2.fillRect(0, 0, image.width, verticalThickness)
    g2.fillRect(0, verticalThickness, horizontalThickness, image.height-verticalThickness)
    g2.fillRect(image.width-horizontalThickness, verticalThickness, image.width, image.height-verticalThickness)
    g2.fillRect(0, image.height-verticalThickness, image.width, image.height)
    g2.dispose()
  }
}

