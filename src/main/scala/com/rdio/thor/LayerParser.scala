package com.rdio.thor

import java.awt.{Color, Font}

import scala.math
import scala.util.parsing.combinator._

case class ColorStop(color: Color, stop: Float)

trait FontNode{}
case class FontPixelsNode(family: String = "Helvetica", size: Int = 12, style: Int = 0) extends FontNode
case class FontPercentNode(family: String = "Helvetica", size: Float = 1.0f, style: Int = 0) extends FontNode

trait ImageNode{}
case class EmptyNode() extends ImageNode
case class UrlNode(url: String) extends ImageNode
case class IndexNode(index: Int) extends ImageNode
case class PreviousNode() extends ImageNode

trait FilterNode{}
case class NoopNode() extends FilterNode { override def toString = "noop" }
case class LinearGradientNode(degrees: Float, colors: List[Color], stops: List[Float]) extends FilterNode { override def toString = "lineargradient" }
case class BlurNode() extends FilterNode { override def toString = "blur" }
case class BoxBlurNode(hRadius: Int, vRadius: Int) extends FilterNode { override def toString = "boxblur" }
case class BoxBlurPercentNode(hRadius: Float, vRadius: Float) extends FilterNode { override def toString = "boxblurpercent" }
case class ColorizeNode(color: Color) extends FilterNode { override def toString = "colorize" }
case class ScaleNode(percentage: Float) extends FilterNode { override def toString = "scale" }
case class ZoomNode(percentage: Float) extends FilterNode { override def toString = "zoom" }
case class ScaleToNode(width: Int, height: Int) extends FilterNode { override def toString = "scaleto" }
case class TextNode(text: String, font: FontNode, color: Color) extends FilterNode { override def toString = "text" }
case class TextPercentNode(text: String, font: FontPercentNode, color: Color) extends FilterNode { override def toString = "textpercent" }
case class GridNode(urls: List[ImageNode]) extends FilterNode { override def toString = "grid" }
case class TextPositionedNode(
  text: String, 
  font: FontNode, 
  color: Color,
  pos: List[ImagePosition], 
  hAlign: HorizontalAlignment, 
  vAlign: VerticalAlignment, 
  fit: TextWidth) extends FilterNode { override def toString = "textpositioned" }
case class PadNode(padding: Int) extends FilterNode { override def toString = "pad" }
case class PadPercentNode(padding: Float) extends FilterNode { override def toString = "padpercent" }
case class RoundCornersNode(radius: Int) extends FilterNode { override def toString = "roundcorners" }
case class RoundCornersPercentNode(radius: Float) extends FilterNode { override def toString = "roundcornerspercent" }
case class OverlayNode(overlay: ImageNode) extends FilterNode { override def toString = "overlay" }
case class MaskNode(overlay: ImageNode, mask: ImageNode) extends FilterNode { override def toString = "mask" }
case class CoverNode() extends FilterNode { override def toString = "cover" }
case class FitNode() extends FilterNode { override def toString = "fit" }
case class FrameNode(thickness: Length, color: Color) extends FilterNode { override def toString = "frame" }

case class LayerNode(source: ImageNode, filter: FilterNode)

class LayerParser(requestWidth: Int, requestHeight: Int) extends JavaTokenParsers {

  // helper for debugging user errors in input strings
  def nameParser[T](parserName: String)(p: => Parser[T]) = Parser{ in =>
    p(in) match {
      case Failure(msg, next) => Failure(s"Error parsing `${parserName}`. ${msg}", next)
      case other => other
    }
  }.named(parserName)

  // a generic list of elements
  def list[A](a: Parser[A]): Parser[List[A]] = "[" ~> repsep(a, ",") <~ "]"

  def singletonOrList[A](a: Parser[A]): Parser[List[A]] = {
    def singleton = a ^^ (List(_))

    list(a) | singleton
  }

  // number - matches an integer or floating point number
  // e.g. `3.14159`
  def number: Parser[Float] = nameParser("""<number> := any floating point number without exponential notation""")(
    """\d+(\.\d+)?""".r ^^ (_.toFloat)
  )

  // integer - matches an integer
  // e.g. `42`
  def integer: Parser[Int] = nameParser("""<integer> := any integer""")(
    """\d+""".r ^^ (_.toInt)
  )

  // degrees - matches a numerical degree
  // e.g. `90deg`
  def degrees: Parser[Float] = nameParser("""<degrees> := <number> "deg" // where the number falls in the range from 0 to 360""")(
    number <~ "deg" ^^ {
      case degrees => (degrees % 360)
    })

  // percent - matches a percentage number
  // e.g. `14%`
  def percent: Parser[Float] = nameParser("""<percent> := <number> "%" // where the number falls in the range from 0 to 100""")(
    number <~ "%" ^^ {
      case percentage => math.min(math.max(percentage / 100.0f, 0.0f), 1.0f)
    })

  // pixels - matches a pixel-unit number
  // e.g. `23px`
  def pixels: Parser[Int] = nameParser("""<pixels> := <number> "px" """)(integer <~ "px")

  def percentOfWidth: Parser[Length] = percent <~ "width" ^^ (LengthPercentWidth(_))
  def percentOfHeight: Parser[Length] = percent <~ "height" ^^ (LengthPercentHeight(_))
  def length: Parser[Length] = nameParser("""<length> := <pixels> | <percent> | <percent> "width" | <percent> "height" """)(
    percentOfWidth | percentOfHeight | (percent ^^ (LengthPercentage(_))) | (pixels ^^ (LengthPixels(_)))
  )

  // url - matches a valid url
  def url: Parser[UrlNode] = nameParser("""<url> := a valid url path to an image, either relative or with a domain""")(
    """(\b((?:https?)://[-a-zA-Z0-9+&@#/%?=~_|!,.]*[-a-zA-Z0-9+&@#/%=~_|])|[\w\-\/\.%]+)""".r ^^ {
      case url => UrlNode(url)
  })

  // empty - matches an empty image placeholder
  // e.g. `_`
  def empty: Parser[EmptyNode] = nameParser("""<empty> := "_"  // an empty image placeholder""")(
    "_" ^^ {
      case _ => EmptyNode()
    })

  // placeholder - matches an image placeholder
  // e.g. `$1`
  def placeholder: Parser[IndexNode] = nameParser("""<index> := "$" <integer>  // the layer at the given index""")(
    "$" ~ integer ^^ {
      case _ ~ index => IndexNode(index)
    }) 

  // source - matches either a path or image placeholder
  def source: Parser[ImageNode] = nameParser("""<source> := <empty> | <url> | <index>  // represents an image layer""")(
    empty | url | placeholder
  )

  // e.g. `bold`
  def fontStyle: Parser[Int] = nameParser("""<fontstyle> := "bold" | "italic" |  "normal" """)(
    """bold|italic|normal""".r ^^ {
      case "normal" => 0
      case "bold" => 1
      case "italic" => 2
    })

  def fontStyles: Parser[Int] = nameParser("""<fontstyles> := <fontstyle> | <fontstyle> " " <fontstyles>  // i.e. a list of <fontstyle>, separated by spaces""")(
    rep1(fontStyle) ^^ { styles =>
      // Sum the styles to a maximum of 3
      math.min(styles.reduceLeft((styles, style) => styles + style), 3)
    })

  // e.g. `"blah"`
  def string: Parser[String] = nameParser("<string> := any quoted string")(
    stringLiteral ^^ { 
      string => string.stripPrefix("\"").stripSuffix("\"")
    })

  // font - matches a font
  // e.g. `bold14pxHelvetica`
  def fontpixels: Parser[FontNode] = (fontStyles?) ~ (pixels?) ~ string ^^ {
    case maybeStyle ~ maybeSize ~ family => {
      val style: Int = maybeStyle.getOrElse(Font.PLAIN)
      val size: Int = maybeSize.getOrElse(12)
      FontPixelsNode(family, size, style).asInstanceOf[FontNode]
    }
  }

  // e.g. `bold50%Courier`
  def fontpercent: Parser[FontNode] = (fontStyles?) ~ (percent?) ~ string ^^ {
    case maybeStyle ~ maybePercentage ~ family => {
      val style: Int = maybeStyle.getOrElse(Font.PLAIN)
      val size: Float = maybePercentage.getOrElse(1.0f)
      FontPercentNode(family, size, style).asInstanceOf[FontNode]
    }
  }

  def font: Parser[FontNode] = nameParser(
    """<font> :=   (<fontstyles> " ")? (<pixels> " ")? <font-family: string>
              | (<fontstyles> " ")? (<percent> " ")? <font-family: string>""")(
    fontpixels | fontpercent)

  // rgba - matches an rgba color with alpha
  // e.g. `rgba(0.5,0.5,0.5,1.0)`
  def rgba: Parser[Color] = nameParser("""<rgba> := "rgba(" <r:number> "," <g:number> "," <b:number> "," <a:number> ")"  // where r,g,b,a are numbers between 0.0 and 1.0""")(
    "rgba(" ~> repsep(number, ",") <~ ")" ^^ {
      case List(r, g, b, a) => {
        val cr = math.min(math.max(r, 0.0), 1.0)
        val cg = math.min(math.max(g, 0.0), 1.0)
        val cb = math.min(math.max(b, 0.0), 1.0)
        val ca = math.min(math.max(a, 0.0), 1.0)
        new Color(cr.toFloat, cg.toFloat, cb.toFloat, ca.toFloat)
      }
      case _ => Color.black
    })

  // rgb - matches an rgb color
  // e.g. `rgb(0.5,0.5,0.5)`
  def rgb: Parser[Color] = nameParser("""<rgb> := "rgb(" <r:number> "," <g:number> "," <b:number> ")"  // where r,g,b,a are numbers between 0.0 and 1.0""")(
    "rgb(" ~> repsep(number, ",") <~ ")" ^^ {
      case List(r, g, b) => {
        val cr = math.min(math.max(r, 0.0), 1.0)
        val cg = math.min(math.max(g, 0.0), 1.0)
        val cb = math.min(math.max(b, 0.0), 1.0)
        new Color(cr.toFloat, cg.toFloat, cb.toFloat)
      }
      case _ => Color.black
    })

  def hsl: Parser[Color] = nameParser("""<hsl> := "hsl(" <h:number> "," <s:number> "," <l:number> ")"  // where h is a number between 0.0 and 360.0, and s,l are numbers between 0.0 and 1.0""")(
    "hsl(" ~> repsep(number, ",") <~ ")" ^^ {
      case List(h, s, l) => {
        val ch = math.min(math.max(h, 0.0), 360.0)
        val cs = math.min(math.max(s, 0.0), 1.0)
        val cl = math.min(math.max(l, 0.0), 1.0)

        val (r,g,b) = hslToRgb(ch.toFloat / 360.0f, cs.toFloat, cl.toFloat)
        new Color(r,g,b)
      }
      case _ => Color.black
    })

  def hsla: Parser[Color] = nameParser("""<hsla> := "hsla(" <h:number> "," <s:number> "," <l:number> "," <a:number> ")"  // where h is a number between 0.0 and 360.0, and s,l are numbers between 0.0 and 1.0""")(
    "hsla(" ~> repsep(number, ",") <~ ")" ^^ {
      case List(h, s, l, a) => {
        val ch = math.min(math.max(h, 0.0), 360.0)
        val cs = math.min(math.max(s, 0.0), 1.0)
        val cl = math.min(math.max(l, 0.0), 1.0)
        val ca = math.min(math.max(a, 0.0), 1.0)

        val (r,g,b) = hslToRgb(ch.toFloat / 360f, cs.toFloat, cl.toFloat)
        new Color(r,g,b, ca.toFloat)
      }
      case _ => Color.black
    })

  def hslToRgb(h: Float, s: Float, l: Float): (Float,Float,Float) = {
    if (s == 0f) {
      (l, l, l) // achromatic
    } else {
      def hue2rgb(p: Float, q: Float, t: Float) = {
        // make sure t is bounded between 0 and 1
        val tp = t match {
          case _ if (t < 0f) =>  t + 1f
          case _ if (t > 1f) =>  t - 1f
          case _ =>              t
        }

        tp match {
          case _ if (tp < 1f/6f) =>  p + (q - p) * 6f * tp
          case _ if (tp < 1f/2f) =>  q
          case _ if (tp < 2f/3f) =>  p + (q - p) * (2f/3f - tp) * 6f
          case _ =>                  p
        }
      }

      val q = l match {
        case _ if (l < 0.5f) =>  l * (1f + s)
        case _ =>                l + s - l * s
      }
      val p = 2f * l - q

      val r = hue2rgb(p, q, h + 1f/3f)
      val g = hue2rgb(p, q, h)
      val b = hue2rgb(p, q, h - 1f/3f)
      (r,g,b)
    }
  }

  // color - matches a color
  def color: Parser[Color] = nameParser("<color> := <rgba> | <rgb> | <hsla> | <hsl>")(
    rgba | rgb | hsla | hsl
  )

  // colorStop - matches a color+number pair
  // e.g. `rgb(0.5,0.5,0.5)50%`
  def colorStop: Parser[ColorStop] = nameParser("""<colorstop> := <color> <percent>""")(
    color ~ percent ^^ {
      case color ~ number => ColorStop(color, number)
    })

  def centered: Parser[ImagePosition] = "centered" ^^ {
    case "centered" => Centered()
  }

  def cartesianRelative: Parser[ImagePosition] = "cartesian(" ~> percent ~ "," ~ percent <~ ")" ^? {
    case percentX ~ _ ~ percentY => CartesianRelative(percentX, percentY)
  }

  def cartesianAbsolute: Parser[ImagePosition] = "cartesian(" ~> pixels ~ "," ~ pixels <~ ")" ^? {
    case dx ~ _ ~ dy => CartesianAbsolute(dx, dy)
  }

  def imagePosition: Parser[ImagePosition] = nameParser("""<imagePosition> := "centered" | "cartesian(" <percent> "," <percent> ")" | "cartesian(" <pixels> "," <pixels> ")" """)(
    centered | cartesianRelative | cartesianAbsolute
  )

  def horizontalAlignment: Parser[HorizontalAlignment] = nameParser("""<hAlign> := "left" | "center" | "right" """)(
    """left|center|right""".r ^^ {
      case "left" => LeftAlign()
      case "center" => CenterAlign()
      case "right" => RightAlign()
    })

  def verticalAlignment: Parser[VerticalAlignment] = nameParser("""<vAlign> := "top" | "center" | "bottom" """)(
    """top|center|bottom""".r ^^ {
      case "top" => TopAlign()
      case "center" => CenterAlign()
      case "bottom" => BottomAlign()
    })

  def textWidth: Parser[TextWidth] = nameParser("""<textWidth> := "fromContent" | "fitted(" <width:pixels> "," <maxFontSize:length> ")" """)({
    def fitted: Parser[TextWidth] = "fitted(" ~> pixels ~ "," ~ length <~ ")" ^? {
      case width ~ _ ~ maxFontSize => WidthFitted(width, maxFontSize).asInstanceOf[TextWidth]
    }

    def fromContent: Parser[TextWidth] = "fromContent" ^^ {_ => WidthFromContent().asInstanceOf[TextWidth] }

    fitted | fromContent
  })

  // linear gradient filter
  def linear: Parser[LinearGradientNode] = nameParser("""<linear> := "linear(" <degrees> "," <varargs list of colorstop> ")"  // draws a linear gradient""")(
    "linear(" ~> degrees ~ "," ~ rep1sep(colorStop, ",") <~ ")" ^? {
      case degrees ~ _ ~ colorStops if colorStops.length > 1 => {
        // We sort the stops because LinearGradientPaint requires it
        val sortedColorStops = colorStops.sortBy {
          case ColorStop(color, stop) => stop
        }
        val stops = sortedColorStops.map {
          case ColorStop(color, stop) => stop
        }
        val colors = sortedColorStops.map {
          case ColorStop(color, stop) => color
        }
        LinearGradientNode(degrees, colors, stops)
      }
    })

  // should we handle different sizes for each section? i.e. top, right, bottom, left?
  def frame: Parser[FrameNode] = nameParser("""<frame> := "frame(" <thickness:length> "," <color> ")"  // draws a frame on top of the layer.""")(
    "frame(" ~> length ~ "," ~ color <~ ")" ^? {
      case thickness ~ _ ~ color => {
        FrameNode(thickness, color)
      }
    })

  // blur filter
  def blur: Parser[BlurNode] = nameParser("""<blur> := "blur()"  // a simple 3x3 weighted box blur.""")(
    "blur()" ^^ {
      case _ => BlurNode()
    })

  // box blur filter
  def boxblur: Parser[BoxBlurNode] = nameParser("""<boxblur> := "boxblur(" <pixels> "," <pixels> ")"  // a box blur filter with 2 iterations.""")(
    "boxblur(" ~> pixels ~ "," ~ pixels <~ ")" ^^ {
      case hRadius ~ _ ~ vRadius => BoxBlurNode(hRadius, vRadius)
    })

  def boxblurpercent: Parser[BoxBlurPercentNode] = nameParser("""<boxblur> := "boxblur(" <percent> "," <percent> ")"  // a box blur filter with 2 iterations.""")(
    "boxblur(" ~> percent ~ "," ~ percent <~ ")" ^^ {
      case hPercent ~ _ ~ vPercent => BoxBlurPercentNode(hPercent, vPercent)
    })

  // colorize filter
  def colorize: Parser[ColorizeNode] = nameParser("""<colorize> := "colorize(" <color> ")"  // fill the layer with the given color.""")(
    "colorize(" ~> color <~ ")" ^^ {
      case color => ColorizeNode(color)
    })

  // zoom filter
  def zoom: Parser[ZoomNode] = nameParser("""<zoom> := "zoom(" <percent> ")"  // like scale, with 100% + the given percentage.""")(
    "zoom(" ~> percent <~ ")" ^^ {
      case percentage => ZoomNode(percentage)
    })

  // scale filter
  def scale: Parser[ScaleNode] = nameParser("""<scale> := "scale(" <percent> ")"  // scale the layer proportionally to the given percent of its original size. scaling is bicubic.""")(
    "scale(" ~> percent <~ ")" ^^ {
      case percentage => ScaleNode(percentage)
    })

  // scaleto filter
  def scaleto: Parser[ScaleToNode] = nameParser("""<scaleto> := "scaleto(" <pixels> "," <pixels> ")"  // resamples the layer to the given size. scaling is bicubic.""")(
    "scaleto(" ~> pixels ~ "," ~ pixels <~ ")" ^^ {
      case width ~ _ ~ height => ScaleToNode(width, height)
    })

  // grid filter
  def grid: Parser[GridNode] = nameParser("""<grid> := "grid(" <varargs list of source> ")"  // tiles the given list of layers""")(
    "grid(" ~> repsep(source, ",") <~ ")" ^^ {
      case urls => GridNode(urls)
    })

  // text filter
  def text: Parser[TextNode] = nameParser("""<text> := "text(" <string> "," <font> "," <color> ")"  // draws the text on the center of the layer""")(
    "text(" ~> string ~ "," ~ font ~ "," ~ color <~ ")" ^^ {
      case text ~ _ ~ font ~ _ ~ color => TextNode(text, font, color)
    })

  def textPositioned: Parser[TextPositionedNode] = nameParser(
    """<text> :=  // draws the text at the given location on the image. imagePositions are additive so that you could, 
           // e.g. place the text 4 pixels below the lower 1/3rd mark on the image, centered horizontally. you
           // can also specify whether the text is left/center/right aligned and top/center/bottom aligned, and
           // you can also specify a maximum width for the text so that it does not overflow.
           "text(" <string> "," <font> "," <color> "," (<imagePosition>|[<imagePosition>]) "," <hAlign> "," <vAlign> "," <textwidth> ")"  """)(
    "text(" ~> string ~ "," ~ font ~ "," ~ color ~ "," ~ singletonOrList(imagePosition) ~ "," ~ horizontalAlignment ~ "," ~ verticalAlignment ~ "," ~ textWidth <~ ")" ^^ {
      case text ~ _ ~ font ~ _ ~ color ~ _ ~ pos ~ _ ~ hAlign ~ _ ~ vAlign ~ _ ~ fit => TextPositionedNode(text, font, color, pos, hAlign, vAlign, fit)
    })

  // round filter
  def round: Parser[RoundCornersNode] = nameParser("""<round> := "round(" <pixels> ")"  // round the corners of the layer""")(
    "round(" ~> pixels <~ ")" ^^ {
      case radius => RoundCornersNode(radius)
    })

  def roundpercent: Parser[RoundCornersPercentNode] = nameParser("""<round> := "round(" <percent> ")"  // round the corners of the layer""")(
    "round(" ~> percent <~ ")" ^^ {
      case percent => RoundCornersPercentNode(percent)
    })

  // mask filter
  def mask: Parser[MaskNode] = nameParser("""<mask> := "mask(" <source> "," <source> ")"  // overlays the first layer on top of the source, using the second image as a mask""")(
    "mask(" ~> source ~ "," ~ source <~ ")" ^^ {
      case overlay ~ _ ~ mask => MaskNode(overlay, mask)
    })

  // overlay filter
  def overlay: Parser[OverlayNode] = nameParser("""<overlay> := "overlay(" <source> ")"  // overlays the given layer on top of the source""")(
    "overlay(" ~> source <~ ")" ^^ {
      case overlay => OverlayNode(overlay)
    })

  // pad filter
  def pad: Parser[PadNode] = nameParser("""<pad> := "pad(" <pixels> ")"  // pads the layer with a transparent border""")(
    "pad(" ~> pixels <~ ")" ^^ {
      case padding => PadNode(padding)
    })

  def padpercent: Parser[PadPercentNode] = nameParser("""<pad> := "pad(" <percent> ")"  // pads the layer with a transparent border""")(
    "pad(" ~> percent <~ ")" ^^ {
      case percent => PadPercentNode(percent)
    })

  // cover filter
  def cover: Parser[CoverNode] = nameParser("""<cover> := "cover()"  // resizes the layer so that it covers the requested width and height. parts of the layer may get chopped off.""")(
    "cover()" ^^ {
      case _ => CoverNode()
    })

  // fit filter
  def fit: Parser[FitNode] = nameParser("""<fit> := "fit()"  // resizes the layer so that it fits the requested width and height. parts of the layer may be empty/transparent.""")(
    "fit()" ^^ {
      case _ => FitNode()
    })

  // all filters
  def filters: Parser[FilterNode] =
    text | textPositioned | linear | boxblur | boxblurpercent |
    blur | scaleto | zoom | scale |
    grid | round | roundpercent | mask |
    cover | fit |
    colorize | overlay | pad | padpercent | frame

  // Match a url without filters
  def sourcelayer = nameParser("sourcelayer")(
    source ^^ ({
      case p => LayerNode(p, NoopNode())
    })
  )

  // Match a url or placeholder with filters
  def sourcewithfilter = nameParser("sourcewithfilter")(
    source ~ ":" ~ filters ^^ ({
      case p ~ _ ~ f => LayerNode(p, f)
    })
  )

  // Match just a filter
  def filterlayer = nameParser("filterlayer")(
    filters ^^ ({
      case f => LayerNode(PreviousNode(), f)
    })
  )

  // layer - matches a single layer
  def layer: Parser[LayerNode] = nameParser("""<layer> := <source> | <source> ":" <filter> | <filter>""")(
    sourcelayer ||| sourcewithfilter ||| filterlayer)

  // layers - matches all layers
  def layers: Parser[List[LayerNode]] = nameParser("""<layers> := <layer> | <layer> ";" <layers> """)(
    rep1sep(layer, ";"))

  def namedPrimitives: List[Parser[Any]] =
    List(
      number, integer, degrees, percent, pixels, length,
      empty, url, placeholder, source,
      string,
      fontStyle, fontStyles, font,
      rgba, rgb, hsla, hsl,
      color, colorStop,
      imagePosition,
      horizontalAlignment, verticalAlignment,
      textWidth
    )

  def namedFilters: List[Parser[Any]] = 
    List(
      linear, frame, blur, boxblur, boxblurpercent, colorize, zoom, scale, scaleto, grid, text, textPositioned, 
      round, roundpercent, mask, overlay, pad, padpercent, cover, fit
    )

  def namedLayers: List[Parser[Any]] =
    List(layer, layers)

  def getParserName[T <: Parser[Any]](p: T): String = {
    val s = p.toString
    s.substring("Parser (".length, s.length-1)
  }
}
