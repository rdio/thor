package com.rdio.thor

import java.awt.Color

import scala.math
import scala.util.parsing.combinator._

case class ColorStop(color: Color, stop: Float)

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
case class ColorizeNode(color: Color) extends FilterNode
case class ScaleNode(percentage: Float) extends FilterNode
case class ZoomNode(percentage: Float) extends FilterNode
case class ScaleToNode(width: Int, height: Int) extends FilterNode
case class GridNode(paths: List[ImageNode]) extends FilterNode
case class PadNode(padding: Int) extends FilterNode
case class ConstrainNode(constraints: List[String]) extends FilterNode
case class CompositeNode(image: ImageNode, composites: List[String]) extends FilterNode
case class RoundCornersNode(radius: Int) extends FilterNode
case class OverlayNode(overlay: ImageNode) extends FilterNode
case class MaskNode(overlay: ImageNode, mask: ImageNode) extends FilterNode
case class CoverNode(width: Int, height: Int) extends FilterNode

case class LayerNode(path: ImageNode, filter: FilterNode)

class LayerParser(width: Int, height: Int) extends RegexParsers {

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
  def path: Parser[PathNode] = """[\w\-\/\.]+""".r ^^ {
    case path => PathNode(path)
  }

  // empty - matches an empty image placeholder
  def empty: Parser[EmptyNode] = "_" ^^ {
    case _ => EmptyNode()
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

  // composites unit
  def composites: Parser[String] =
    "average" | "blue" | "color" | "colorburn" | "colordodge" | "diff" | "green" |
    "grow" | "hue" | "hard" | "heat" | "lighten" | "negation" | "luminosity" |
    "multiply" | "negation" | "normal" | "overlay" | "red" | "reflect" | "saturation" |
    "screen" | "subtract"

  // constraints unit
  def constraints: Parser[String] = "left" | "right" | "top" | "bottom"

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
  } | "boxblur(" ~> percent ~ "," ~ percent <~ ")" ^^ {
    case hPercent ~ _ ~ vPercent => {
      BoxBlurNode((hPercent * width.toFloat).toInt, (vPercent * height.toFloat).toInt)
    }
  }

  // colorize filter
  def colorize: Parser[ColorizeNode] = "colorize(" ~> color <~ ")" ^^ {
    case color => ColorizeNode(color)
  }

  // zoom filter
  def zoom: Parser[ZoomNode] = "zoom(" ~> percent <~ ")" ^^ {
    case percentage => ZoomNode(percentage)
  }

  // constrain filter
  def constrain: Parser[ConstrainNode] = "constrain(" ~> rep1sep(constraints, ",") <~ ")" ^^ {
    case constraints => ConstrainNode(constraints)
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

  // round filter
  def round: Parser[RoundCornersNode] = "round(" ~> pixels <~ ")" ^^ {
    case radius => RoundCornersNode(radius)
  } | "round(" ~> percent <~ ")" ^^ {
    case percent => RoundCornersNode((percent * math.max(width, height).toFloat).toInt)
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
  } | "pad(" ~> percent <~ ")" ^^ {
    case percent => PadNode((percent * math.max(width, height).toFloat).toInt)
  }

  // composite filter
  def composite: Parser[CompositeNode] = "composite(" ~> source ~ "," ~ rep1sep(composites, ",") <~ ")" ^^ {
    case source ~ _ ~ composites => CompositeNode(source, composites)
  }

  // cover filter
  def cover: Parser[CoverNode] = "cover(" ~> pixels ~ "," ~ pixels <~ ")" ^^ {
    case width ~ _ ~ height => CoverNode(width, height)
  }

  // all filters
  def filters: Parser[FilterNode] = linear | boxblur | blur | scaleto | zoom | scale | grid | round | mask | colorize | overlay | composite | constrain | pad

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
