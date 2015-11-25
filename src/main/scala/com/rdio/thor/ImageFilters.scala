package com.rdio.thor

import scala.annotation.tailrec

import java.awt.{Font, FontMetrics, AlphaComposite, Color, Graphics2D, RenderingHints, LinearGradientPaint, MultipleGradientPaint, Rectangle}
import java.awt.geom.{RoundRectangle2D, AffineTransform, Point2D, Rectangle2D}
import java.awt.font.{FontRenderContext, GlyphVector}
import BaseUtils._

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

sealed trait TextFit
case class WidthFromContent() extends TextFit
case class WidthFitted(fitTo: Length, maxFontSize: Length) extends TextFit
case class WidthAndHeightFitted(fitWidthTo: Length, fitHeightTo: Length, maxFontSize: Length) extends TextFit

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

  def getLengthInPixelsForImage(image: Image, l: Length): Int = l match {
    case LengthPercentHeight(percent) => (image.height * percent).round
    case LengthPercentWidth(percent) => (image.width * percent).round
    case LengthPercentage(percent) => (percent * math.sqrt(image.width * image.width + image.height * image.height)).round.toInt
    case LengthPixels(px) => px
  }

  def getLengthInPixelsForText(lineHeightPx: Int, image: Image)(l: Length): Int = l match {
    case LengthPercentHeight(percent) => (image.height * percent).round
    case LengthPercentWidth(percent) => (image.width * percent).round
    case LengthPercentage(percent) => (percent * lineHeightPx).round
    case LengthPixels(px) => px
  }
}

// auxiliaries for parsing text options
case class TextOptions(bgColor: Option[Color],
                       paddingTop: Option[Length],
                       paddingRight: Option[Length],
                       paddingBottom: Option[Length],
                       paddingLeft: Option[Length]
                      ) {

  def merge(other: TextOptions): TextOptions = {
    TextOptions(
      mergeOpt(bgColor, other.bgColor),
      mergeOpt(paddingTop, other.paddingTop),
      mergeOpt(paddingRight, other.paddingRight),
      mergeOpt(paddingBottom, other.paddingBottom),
      mergeOpt(paddingLeft, other.paddingLeft)
    )
  }
}


object TextOptions {
  val empty: TextOptions = TextOptions(None, None, None, None, None)
}

/*
object TextFilterTest {
  import TextUtils._
  val font = Font.createFont(Font.PLAIN, new java.io.File("/users/achan/Library/Fonts/Jan Fromm - Rooney-Regular.otf"))
  val fontSize = 16
  val text = "zum schreiben, auf zeichnung"
  val image = Image.filled(400, 400)
  val g2 = image.awt.getGraphics.asInstanceOf[Graphics2D]
  val (overallWidth, lineHeight) = getDimensionsOfText(font, g2, text, fontSize)
  val textWords = text.split(" ")
  val numWords = textWords.length
  val wordWidths = textWords.map(str => getWidthOfText(font, g2, str, fontSize))
  val wordWidthsTotal = wordWidths.sum

  val textTokens = textWords.zip(wordWidths)
}
*/

object TextUtils {
  def layoutText(g2: Graphics2D, x: Int, y: Int,
                 horizontalAlignment: HorizontalAlignment, verticalAlignment: VerticalAlignment,
                 lines: List[(String, Int)], fontSize: Int, lineHeight: Float)
  : (Int, Int, Int, Int, List[(String, Int, Int)]) = {

    val overallWidth = lines.map({ case (l, width) => width }).max
    val overallHeight = Math.ceil(lineHeight * fontSize * lines.length).toInt
    val alignmentFxn: (((String,Int), Int)) => (String, Int, Int) = alignWithBounds(x, y, horizontalAlignment, verticalAlignment, fontSize, lineHeight)
    val linesWithIndices: List[((String,Int), Int)] = lines.zipWithIndex
    val linesWithPositions = linesWithIndices.map(alignmentFxn)

    val overallX = horizontalAlignment match {
      case LeftAlign() => x
      case CenterAlign() => x - overallWidth / 2
      case RightAlign() => x - overallWidth
    }
    val overallY = verticalAlignment match {
      case TopAlign() => y // - visualBounds.y
      case CenterAlign() => y - overallHeight / 2 // - visualBounds.y
      case BottomAlign() => y - overallHeight // - visualBounds.y
    }

    (overallX, overallY, overallWidth, overallHeight, linesWithPositions)
  }

  def alignWithBounds(x: Int, y: Int,
                      horizontalAlignment: HorizontalAlignment, verticalAlignment: VerticalAlignment,
                      fontSize: Int, lineHeight: Float)
  : ((((String, Int), Int)) => (String, Int, Int)) = {

    case ((line, width), lineNumber) => {
      val height = fontSize * lineHeight

      val textX = horizontalAlignment match {
        case LeftAlign() => x
        case CenterAlign() => x - width / 2
        case RightAlign() => x - width
      }
      val yOffset = Math.round(y + height * lineNumber)
      val textY = verticalAlignment match {
        case TopAlign() => yOffset // - visualBounds.y
        case CenterAlign() => yOffset - height / 2 // - visualBounds.y
        case BottomAlign() => yOffset - height // - visualBounds.y
      }

      (line, textX, textY.toInt)
    }
  }

  def getTextLines(image: Image, g2: Graphics2D, font: Font,
                   textWidth: TextFit, text: String, lineHeight: Float): (Int, List[(String, Int)]) = {
    import Length._

    textWidth match {
      case WidthFromContent() => {
        (font.getSize, List((text, getWidthOfText(font, g2, text, font.getSize))))
      }
      case WidthFitted(fitTo, maxFontSize) => {
        val maxFontSizeInPixels = getLengthInPixelsForImage(image, maxFontSize)
        val widthInPixels = getLengthInPixelsForImage(image, fitTo)
        val fontSize = fitTextToWidth(font, g2, text, widthInPixels, maxFontSizeInPixels)
        (fontSize, List((text, getWidthOfText(font, g2, text, fontSize))))
      }
      case WidthAndHeightFitted(fitWidthTo, fitHeightTo, maxFontSize) => {
        val maxFontSizeInPixels = getLengthInPixelsForImage(image, maxFontSize)
        val widthInPixels = getLengthInPixelsForImage(image, fitWidthTo)
        val heightInPixels = getLengthInPixelsForImage(image, fitHeightTo)
        breakTextIntoLines(font, g2, text, widthInPixels, heightInPixels, lineHeight, maxFontSizeInPixels)
      }
    }
  }

  def breakTextIntoLines(font: Font, g2: Graphics2D, text: String, width: Int, height: Int,
                         lineHeightMultiplier: Float, maxFontSize: Int): (Int, List[(String, Int)]) = {
    val overallWidth = getWidthOfText(font, g2, text, maxFontSize)
    val textWords = text.split(" ")
    val numWords = textWords.length
    val wordWidths = textWords.map(str => getWidthOfText(font, g2, str, maxFontSize))
    val wordWidthsTotal = wordWidths.sum

    val textTokens = textWords.zip(wordWidths)

    if (overallWidth <= width) {
      fitLinesToBox(font, g2, List(text), maxFontSize, width, height, lineHeightMultiplier)

    } else {
      val (currentFontSize, currentBestFit) =
        fitLinesToBox(font, g2, List(text), maxFontSize, width, height, lineHeightMultiplier)

      @tailrec
      def searchForBestLineBreaks(currentNumLines: Int, currentFontSize: Int, currentBestFit: List[(String, Int)])
      : (Int, List[(String, Int)]) = {
        val lines = breakWordsIntoLines(textTokens, wordWidthsTotal, currentNumLines)
        val (fontSize, linesWithWidths) = fitLinesToBox(font, g2, lines, maxFontSize, width, height, lineHeightMultiplier)

        (fontSize <= currentFontSize, currentNumLines == numWords) match {
          case (true, _) => (currentFontSize, currentBestFit)
          case (false, true) => (fontSize, linesWithWidths)
          case (false, false) => searchForBestLineBreaks(currentNumLines + 1, fontSize, linesWithWidths)
        }
      }

      searchForBestLineBreaks(2, currentFontSize, currentBestFit)
    }
  }

  def fitLinesToBox(font: Font, g2: Graphics2D,
                    lines: List[String], maxFontSize: Int,
                    boundingWidth: Int, boundingHeight: Int, lineHeightMultiplier: Float): (Int, List[(String, Int)]) = {
    val sizes = lines.map(currentLine => fitTextToWidth(font, g2, currentLine, boundingWidth, maxFontSize))
    val overallSizeByWidth = sizes.min
    val overallSize =
      overallSizeByWidth * lineHeightMultiplier * lines.length > boundingHeight match {
        case true =>
          (boundingHeight / (lines.length * lineHeightMultiplier)).toInt
        case false =>
          overallSizeByWidth
      }

    val widths = lines.map(line => getWidthOfText(font, g2, line, overallSize))
    (overallSize, lines.zip(widths))
  }

  def breakWordsIntoLines(textTokens: Iterable[(String, Int)], wordWidthsTotal: Int, numLines: Int): List[String] = {
    var lines = List[List[String]]()
    var currentWordsWidth = 0
    var currentLineIndex = 0
    var currentLine = List[String]()
    var currentTargetWidth = (currentLineIndex + 1) * wordWidthsTotal / numLines

    for ((word, width) <- textTokens) {
      if (currentWordsWidth + width > currentTargetWidth) {
        val excessWidth = (currentWordsWidth + width) - currentTargetWidth
        if (excessWidth > width / 2) {
          currentWordsWidth += width
          lines = currentLine.reverse :: lines
          currentLine = List[String](word)

        } else {
          currentWordsWidth += width
          lines = (word :: currentLine).reverse :: lines
          currentLine = List[String]()
        }

        currentLineIndex += 1
        currentTargetWidth = (currentLineIndex + 1) * wordWidthsTotal / numLines

      } else {
        currentWordsWidth += width
        currentLine = word :: currentLine
      }
    }

    if (currentLine.nonEmpty) {
      lines = currentLine.reverse :: lines
    }

    lines.reverse.map(words => words.mkString(" "))
  }

  def getWidthOfText(font: Font, g2: Graphics2D, text: String, fontSize: Int): Int = {
    val savedFont = g2.getFont
    val test = font.deriveFont(font.getStyle, fontSize)
    g2.setFont(test)
    val width = getRectViaFontMetrics(g2, text).width
    g2.setFont(savedFont)

    width
  }

  def getDimensionsOfText(font: Font, g2: Graphics2D, text: String, fontSize: Int): (Int, Int) = {
    val savedFont = g2.getFont
    val test = font.deriveFont(font.getStyle, fontSize)
    g2.setFont(test)
    val width = getRectViaFontMetrics(g2, text).width
    val height = getRectViaGlyphVector(g2, text).height
    g2.setFont(savedFont)

    (width, height)
  }

  def getRectViaFontMetrics(g2: Graphics2D, text: String): Rectangle = {
    val fontMetrics: FontMetrics = g2.getFontMetrics
    fontMetrics.getStringBounds(text, g2).getBounds
  }

  def getRectViaGlyphVector(g2: Graphics2D, text: String): Rectangle = {
    val graphicsFont: Font = g2.getFont
    val renderContext: FontRenderContext = g2.getFontRenderContext
    val glyphVector: GlyphVector = graphicsFont.createGlyphVector(renderContext, text)
    glyphVector.getVisualBounds.getBounds
  }

  // Lets say you want to add the name of a band to some station image, but you
  // don't want the text to overflow off the image. This function uses binary
  // search to find the largest font size that fits within the given width.
  def fitTextToWidth(font: Font, g2: Graphics2D, text: String, width: Int, maxFontSize: Int): Int = {
    val minSize = 4
    if (getWidthOfText(font, g2, text, minSize) > width) {
      // oy...
      minSize

    } else if (getWidthOfText(font, g2, text, maxFontSize) <= width) {
      maxFontSize

    } else {
      @tailrec
      def search(lo: Int, hi: Int): Int = {
        val mid = (hi + lo) / 2
        mid match {
          case _ if lo == mid =>
            lo

          case _ if width < getWidthOfText(font, g2, text, mid) =>
            search(lo, mid)

          case _ =>
            search(mid, hi)
        }
      }

      search(minSize, maxFontSize)
    }
  }

  def getBackgroundRect(image: Image,
                        overallX: Int, overallY: Int, overallWidth: Int, overallHeight: Int,
                        fontSize: Int, lineHeight: Float, textOptions: TextOptions)
  : (Int, Int, Int, Int) = {

    val defaultPadding = (0.618 * fontSize).round.toInt
    val getLengthFxn = Length.getLengthInPixelsForText(fontSize, image) _
    val padTop =    textOptions.paddingTop.map(getLengthFxn).getOrElse(defaultPadding)
    val padRight =  textOptions.paddingRight.map(getLengthFxn).getOrElse(defaultPadding)
    val padBottom = textOptions.paddingBottom.map(getLengthFxn).getOrElse(defaultPadding)
    val padLeft =   textOptions.paddingLeft.map(getLengthFxn).getOrElse(defaultPadding)

    (overallX - padLeft,
     overallY - padTop - (fontSize * lineHeight).toInt,
     overallWidth + padLeft + padRight,
     overallHeight + padTop + padBottom)
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
                  textWidth: TextFit,
                  textOptions: TextOptions) extends Filter
{
  import Length._
  import TextUtils._

  def apply(image: Image) {
    val lineHeight = 1.382f

    val g2 = image.awt.getGraphics.asInstanceOf[Graphics2D]
    g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

    // need: list of text lines, i.e. List[String], line height in pixels, length of each line in pixels
    val (fontSize, lines) = getTextLines(image, g2, font, textWidth, text, lineHeight)
    g2.setFont(font.deriveFont(font.getStyle, fontSize))

    val (x,y) = imagePos.map(imagePositionToCoords(image)).reduce((a: (Int,Int), b: (Int,Int)) => (a,b) match {
      case ((x1,y1), (x2, y2)) => (x1 + x2, y1 + y2)
    })

    // if textX or textY are outside the image bounds, then the text bocomes cropped,
    // but it draws correctly where you'd expect it to. this is the desired behavior,
    // because what if the designers wanted the text to flow in for artistic effect?
    val (overallX, overallY, overallWidth, overallHeight, linesWithPositions) =
      layoutText(g2, x, y, horizontalAlignment, verticalAlignment, lines, fontSize, lineHeight)

    textOptions.bgColor match {
      case Some(bgColorVal) => {
        val (rx, ry, rw, rh) = getBackgroundRect(image,
          overallX, overallY, overallWidth, overallHeight,
          fontSize, lineHeight, textOptions)

        g2.setColor(bgColorVal)
        g2.fillRect(rx, ry, rw, rh)
      }
      case None => ()
    }
    g2.setColor(color)
    for (elem <- linesWithPositions) {
      val (text, textX, textY) = elem
      g2.drawString(text, textX, textY)
    }
    g2.dispose()
  }

  def imagePositionToCoords(image: Image)(imagePos: ImagePosition): (Int, Int) = imagePos match {
    case Centered() => (image.width / 2, image.height / 2)
    case CartesianAbsolute(x,y) => (x,y)
    case CartesianRelative(percentX, percentY) => ((image.width * percentX).toInt, (image.height * percentY).toInt)
  }

  def align(g2: Graphics2D, text: String, x: Int, y: Int,
            horizontalAlignment: HorizontalAlignment,
            verticalAlignment: VerticalAlignment): (Int, Int, Int, Int, Int) = {
    val stringBounds = getRectViaFontMetrics(g2, text)
    val visualBounds = getRectViaGlyphVector(g2, text)
    val textX = horizontalAlignment match {
      case LeftAlign() => x
      case CenterAlign() => x - stringBounds.width / 2
      case RightAlign() => x - stringBounds.width
    }
    val textY = verticalAlignment match {
      case TopAlign() => y // - visualBounds.y
      case CenterAlign() => y - visualBounds.height / 2 // - visualBounds.y
      case BottomAlign() => y - visualBounds.height // - visualBounds.y
    }

    (textX, textY, stringBounds.width, visualBounds.height, visualBounds.height)
  }
}

object TextFilter {
  def apply(text: String, font: Font, color: Color, imagePos: List[ImagePosition],
            horizontalAlignment: HorizontalAlignment, verticalAlignment: VerticalAlignment,
            textWidth: TextFit, textOptions: TextOptions): Filter =
    new TextFilter(text, font, color, imagePos, horizontalAlignment, verticalAlignment, textWidth, textOptions)

  def apply(text: String, font: Font, color: Color, imagePos: List[ImagePosition],
            horizontalAlignment: HorizontalAlignment, verticalAlignment: VerticalAlignment,
            textWidth: TextFit): Filter =
    new TextFilter(text, font, color, imagePos, horizontalAlignment, verticalAlignment, textWidth, TextOptions.empty)

  def apply(text: String, font: Font, color: Color, imagePos: List[ImagePosition],
            horizontalAlignment: HorizontalAlignment, verticalAlignment: VerticalAlignment): Filter =
    new TextFilter(text, font, color, imagePos, horizontalAlignment, verticalAlignment, WidthFromContent(), TextOptions.empty)

  def apply(text: String, font: Font, color: Color): Filter =
    new TextFilter(text, font, color, List(Centered()), CenterAlign(), CenterAlign(), WidthFromContent(), TextOptions.empty)
}

class FillRectFilter(fillColor: Color, x: Length, y: Length, w: Length, h: Length,
                     horizontalAlignment: HorizontalAlignment, verticalAlignment: VerticalAlignment) extends Filter {
  import Length._

  def apply(image: Image) = {
    val g2 = image.awt.getGraphics.asInstanceOf[Graphics2D]

    val xOffsetPx = getLengthInPixelsForImage(image, x)
    val yOffsetPx = getLengthInPixelsForImage(image, y)
    val widthPx = getLengthInPixelsForImage(image, w)
    val heightPx = getLengthInPixelsForImage(image, h)

    val xPx = horizontalAlignment match {
      case LeftAlign() => xOffsetPx
      case CenterAlign() => xOffsetPx - (widthPx / 2)
      case RightAlign() => xOffsetPx - widthPx
    }

    val yPx = verticalAlignment match {
      case TopAlign() => yOffsetPx
      case CenterAlign() => yOffsetPx - (heightPx / 2)
      case BottomAlign() => yOffsetPx - heightPx
    }

    g2.setColor(fillColor)
    g2.fillRect(xPx, yPx, widthPx, heightPx)
  }
}

object FillRectFilter {
  def apply(fillColor: Color, x: Length, y: Length, w: Length, h: Length,
            horizontalAlignment: HorizontalAlignment, verticalAlignment: VerticalAlignment): FillRectFilter =
    new FillRectFilter(fillColor, x, y, w, h, horizontalAlignment, verticalAlignment)
}

/** Blends between two images using a mask */
class MaskFilter(overlay: Image, mask: Image) extends Filter {
  def apply(image: Image) {
    val imageRaster = image.awt.getRaster
    val overlayRaster = overlay.awt.getRaster

    // composeThroughMask looks at the opacity of the maskRaster but we're
    // going to translate that from a grayscale version of the mask where
    // white is opacity 100% and black opacity 0%
    // Copying the image so we don't desaturate the actual mask layer
    val maskCopy = mask.copy
    val maskRaster = maskCopy.awt.getRaster
    var pixels:Array[Double] = null
    pixels = maskRaster.getPixels(0, 0, maskRaster.getWidth, maskRaster.getHeight, pixels)
    for( i <- pixels.indices by 4) {
        // common grayscale equation. See http://www.thewonderoflight.com/articles/definitive-guide-to-bw-conversion/
        val desaturated = pixels(i) * 0.3 + pixels(i + 1) * 0.59 + pixels(i + 2) * 0.11
        pixels.update(i + 3, desaturated)
    }
    maskRaster.setPixels(0, 0, maskRaster.getWidth, maskRaster.getHeight, pixels)

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

