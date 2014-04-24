package com.rdio.thor

import java.awt.{Color, Font}

import scala.math
import scala.util.parsing.combinator._

case class ColorStop(color: Color, stop: Float)

case class FontNode(family: String = "Helvetica", size: Int = 12, style: Int = 0)

trait ImageNode{}
case class EmptyNode() extends ImageNode
case class PathNode(path: String) extends ImageNode
case class IndexNode(index: Int) extends ImageNode
case class PreviousNode() extends ImageNode

trait FilterNode{}
case class NoopNode() extends FilterNode
case class LinearGradientNode(degrees: Float, colors: List[Color], stops: List[Float]) extends FilterNode
case class BlurNode() extends FilterNode
case class BoxBlurNode(hRadius: Int, vRadius: Int) extends FilterNode
case class BoxBlurPercentNode(hRadius: Float, vRadius: Float) extends FilterNode
case class ColorizeNode(color: Color) extends FilterNode
case class ScaleNode(percentage: Float) extends FilterNode
case class ZoomNode(percentage: Float) extends FilterNode
case class ScaleToNode(width: Int, height: Int) extends FilterNode
case class TextNode(text: String, font: FontNode, color: Color) extends FilterNode
case class GridNode(paths: List[ImageNode]) extends FilterNode
case class PadNode(padding: Int) extends FilterNode
case class PadPercentNode(padding: Float) extends FilterNode
case class RoundCornersNode(radius: Int) extends FilterNode
case class RoundCornersPercentNode(radius: Float) extends FilterNode
case class OverlayNode(overlay: ImageNode) extends FilterNode
case class MaskNode(overlay: ImageNode, mask: ImageNode) extends FilterNode
case class CoverNode(width: Int, height: Int) extends FilterNode

case class LayerNode(path: ImageNode, filter: FilterNode)

class LayerParser(width: Int, height: Int) extends JavaTokenParsers {

  // number - matches an integer or floating point number
  def number: Parser[Float] = """\d+(\.\d+)?""".r ^^ (_.toFloat)

  // integer - matches an integer
  def integer: Parser[Int] = """\d+""".r ^^ (_.toInt)

  // degrees - matches a numerical degree
  def degrees: Parser[Float] = number <~ "deg" ^^ {
    case degrees => (degrees % 360)
  }

  // percent - matches a percentage number
  def percent: Parser[Float] = number <~ "%" ^^ {
    case percentage => math.min(math.max(percentage / 100.0f, 0.0f), 1.0f)
  }

  // pixels - matches a pixel-unit number
  def pixels: Parser[Int] = integer <~ "px"

  // path - matches a valid url path
  def path: Parser[PathNode] = """[\w\-\/\.%]+""".r ^^ {
    case path => PathNode(path)
  }

  // empty - matches an empty image placeholder
  def empty: Parser[EmptyNode] = "_" ^^ {
    case _ => EmptyNode()
  }

  def fontStyle: Parser[Int] = """bold|italic|normal""".r ^^ {
    case "normal" => 0
    case "bold" => 1
    case "italic" => 2
  }

  def fontStyles: Parser[Int] = rep1(fontStyle) ^^ { styles =>
    // Sum the styles to a maximum of 3
    math.min(styles.reduceLeft((styles, style) => styles + style), 3)
  }

  def string: Parser[String] = stringLiteral ^^ { string =>
    string.stripPrefix("\"").stripSuffix("\"")
  }

  // font - matches a font
  def font: Parser[FontNode] = (fontStyles?) ~ (pixels?) ~ string ^^ {
    case maybeStyle ~ maybeSize ~ family => {
      val style: Int = maybeStyle.getOrElse(Font.PLAIN)
      val size: Int = maybeSize.getOrElse(12)
      FontNode(family, size, style)
    }
  }

  // placeholder - matches an image placeholder
  def placeholder: Parser[IndexNode] = "$" ~ integer ^^ {
    case _ ~ index => IndexNode(index)
  }

  // source - matches either a path or image placeholder
  def source: Parser[ImageNode] = empty | path | placeholder

  // rgba - matches an rgba color with alpha
  def rgba: Parser[Color] = "rgba(" ~> repsep(number, ",") <~ ")" ^^ {
    case List(r, g, b, a) => {
      val cr = math.min(math.max(r, 0.0), 1.0)
      val cg = math.min(math.max(g, 0.0), 1.0)
      val cb = math.min(math.max(b, 0.0), 1.0)
      val ca = math.min(math.max(a, 0.0), 1.0)
      new Color(cr.toFloat, cg.toFloat, cb.toFloat, ca.toFloat)
    }
    case _ => Color.black
  }

  // rgb - matches an rgb color
  def rgb: Parser[Color] = "rgb(" ~> repsep(number, ",") <~ ")" ^^ {
    case List(r, g, b) => {
      val cr = math.min(math.max(r, 0.0), 1.0)
      val cg = math.min(math.max(g, 0.0), 1.0)
      val cb = math.min(math.max(b, 0.0), 1.0)
      new Color(cr.toFloat, cg.toFloat, cb.toFloat)
    }
    case _ => Color.black
  }

  // color - matches a color
  def color: Parser[Color] = rgba | rgb

  // colorStop - matches a color+number pair
  def colorStop: Parser[ColorStop] = color ~ percent ^^ {
    case color ~ number => ColorStop(color, number)
  }

  // linear gradient filter
  def linear: Parser[LinearGradientNode] = "linear(" ~> degrees ~ "," ~ rep1sep(colorStop, ",") <~ ")" ^? {
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
  }

  // blur filter
  def blur: Parser[BlurNode] = "blur()" ^^ {
    case _ => BlurNode()
  }

  // box blur filter
  def boxblur: Parser[BoxBlurNode] = "boxblur(" ~> pixels ~ "," ~ pixels <~ ")" ^^ {
    case hRadius ~ _ ~ vRadius => BoxBlurNode(hRadius, vRadius)
  }

  def boxblurpercent: Parser[BoxBlurPercentNode] = "boxblur(" ~> percent ~ "," ~ percent <~ ")" ^^ {
    case hPercent ~ _ ~ vPercent => BoxBlurPercentNode(hPercent, vPercent)
  }

  // colorize filter
  def colorize: Parser[ColorizeNode] = "colorize(" ~> color <~ ")" ^^ {
    case color => ColorizeNode(color)
  }

  // zoom filter
  def zoom: Parser[ZoomNode] = "zoom(" ~> percent <~ ")" ^^ {
    case percentage => ZoomNode(percentage)
  }

  // scale filter
  def scale: Parser[ScaleNode] = "scale(" ~> percent <~ ")" ^^ {
    case percentage => ScaleNode(percentage)
  }

  // scaleto filter
  def scaleto: Parser[ScaleToNode] = "scaleto(" ~> pixels ~ "," ~ pixels <~ ")" ^^ {
    case width ~ _ ~ height => ScaleToNode(width, height)
  }

  // grid filter
  def grid: Parser[GridNode] = "grid(" ~> repsep(source, ",") <~ ")" ^^ {
    case paths => GridNode(paths)
  }

  // text filter
  def text: Parser[TextNode] = "text(" ~> string ~ "," ~ font ~ "," ~ color <~ ")" ^^ {
    case text ~ _ ~ font ~ _ ~ color => TextNode(text, font, color)
  }

  // round filter
  def round: Parser[RoundCornersNode] = "round(" ~> pixels <~ ")" ^^ {
    case radius => RoundCornersNode(radius)
  }

  def roundpercent: Parser[RoundCornersPercentNode] = "round(" ~> percent <~ ")" ^^ {
    case percent => RoundCornersPercentNode(percent)
  }

  // mask filter
  def mask: Parser[MaskNode] = "mask(" ~> source ~ "," ~ source <~ ")" ^^ {
    case overlay ~ _ ~ mask => MaskNode(overlay, mask)
  }

  // overlay filter
  def overlay: Parser[OverlayNode] = "overlay(" ~> source <~ ")" ^^ {
    case overlay => OverlayNode(overlay)
  }

  // pad filter
  def pad: Parser[PadNode] = "pad(" ~> pixels <~ ")" ^^ {
    case padding => PadNode(padding)
  }

  def padpercent: Parser[PadPercentNode] = "pad(" ~> percent <~ ")" ^^ {
    case percent => PadPercentNode(percent)
  }

  // cover filter
  def cover: Parser[CoverNode] = "cover(" ~> pixels ~ "," ~ pixels <~ ")" ^^ {
    case width ~ _ ~ height => CoverNode(width, height)
  }

  // all filters
  def filters: Parser[FilterNode] =
    text | linear | boxblur | boxblurpercent |
    blur | scaleto | zoom | scale |
    grid | round | roundpercent | mask |
    colorize | overlay | pad | padpercent

  // layer - matches a single layer
  def layer: Parser[LayerNode] =
    // Match a path without filters
    source ^^ ({
      case p => LayerNode(p, NoopNode())
    }) ||| // or, match a path or placeholder with filters
    source ~ ":" ~ filters ^^ ({
      case p ~ _ ~ f => LayerNode(p, f)
    }) ||| // or, match just a filter
    filters ^^ ({
      case f => LayerNode(PreviousNode(), f)
    })

  // layers - matches all layers
  def layers: Parser[List[LayerNode]] = rep1sep(layer, ";")
}
