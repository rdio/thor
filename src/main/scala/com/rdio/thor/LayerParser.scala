package com.rdio.thor

import java.awt.{Color, Font}

import scala.math
import scala.util.parsing.combinator._

case class ColorStop(color: Color, stop: Float)

case class FontNode(family: String = "Helvetica", size: Int = 12, style: Int = 0)
case class FontPercentNode(family: String = "Helvetica", size: Float = 1.0f, style: Int = 0)

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
case class PadNode(padding: Int) extends FilterNode { override def toString = "pad" }
case class PadPercentNode(padding: Float) extends FilterNode { override def toString = "padpercent" }
case class RoundCornersNode(radius: Int) extends FilterNode { override def toString = "roundcorners" }
case class RoundCornersPercentNode(radius: Float) extends FilterNode { override def toString = "roundcornerspercent" }
case class OverlayNode(overlay: ImageNode) extends FilterNode { override def toString = "overlay" }
case class MaskNode(overlay: ImageNode, mask: ImageNode) extends FilterNode { override def toString = "mask" }
case class CoverNode() extends FilterNode { override def toString = "cover" }
case class FitNode() extends FilterNode { override def toString = "fit" }

case class LayerNode(source: ImageNode, filter: FilterNode)

class LayerParser(requestWidth: Int, requestHeight: Int) extends JavaTokenParsers {

  // helper for debugging user errors in input strings
  def nameParser[T](parserName: String)(p: => Parser[T]) = Parser{ in =>
    p(in) match {
      case Failure(msg, next) => Failure(s"Error parsing `${parserName}`. ${msg}", next)
      case other => other
    }
  }.named(parserName)

  // number - matches an integer or floating point number
  // e.g. `3.14159`
  def number: Parser[Float] = """\d+(\.\d+)?""".r ^^ (_.toFloat)

  // integer - matches an integer
  // e.g. `42`
  def integer: Parser[Int] = """\d+""".r ^^ (_.toInt)

  // degrees - matches a numerical degree
  // e.g. `90deg`
  def degrees: Parser[Float] = number <~ "deg" ^^ {
    case degrees => (degrees % 360)
  }

  // percent - matches a percentage number
  // e.g. `14%`
  def percent: Parser[Float] = number <~ "%" ^^ {
    case percentage => math.min(math.max(percentage / 100.0f, 0.0f), 1.0f)
  }

  // pixels - matches a pixel-unit number
  // e.g. `23px`
  def pixels: Parser[Int] = integer <~ "px"

  // url - matches a valid url
  def url: Parser[UrlNode] = """(\b((?:https?)://[-a-zA-Z0-9+&@#/%?=~_|!,.]*[-a-zA-Z0-9+&@#/%=~_|])|[\w\-\/\.%]+)""".r ^^ {
    case url => UrlNode(url)
  }

  // empty - matches an empty image placeholder
  // e.g. `_`
  def empty: Parser[EmptyNode] = "_" ^^ {
    case _ => EmptyNode()
  }

  // e.g. `bold`
  def fontStyle: Parser[Int] = """bold|italic|normal""".r ^^ {
    case "normal" => 0
    case "bold" => 1
    case "italic" => 2
  }

  // this makes no sense to me... it seems to match
  // e.g. `bolditalic`
  def fontStyles: Parser[Int] = rep1(fontStyle) ^^ { styles =>
    // Sum the styles to a maximum of 3
    math.min(styles.reduceLeft((styles, style) => styles + style), 3)
  }

  // e.g. `"blah"`
  def string: Parser[String] = stringLiteral ^^ { string =>
    string.stripPrefix("\"").stripSuffix("\"")
  }

  // font - matches a font
  // e.g. `bold14pxHelvetica`
  def font: Parser[FontNode] = (fontStyles?) ~ (pixels?) ~ string ^^ {
    case maybeStyle ~ maybeSize ~ family => {
      val style: Int = maybeStyle.getOrElse(Font.PLAIN)
      val size: Int = maybeSize.getOrElse(12)
      FontNode(family, size, style)
    }
  }

  // e.g. `bold50%Courier`
  def fontpercent: Parser[FontPercentNode] = (fontStyles?) ~ (percent?) ~ string ^^ {
    case maybeStyle ~ maybePercentage ~ family => {
      val style: Int = maybeStyle.getOrElse(Font.PLAIN)
      val size: Float = maybePercentage.getOrElse(1.0f)
      FontPercentNode(family, size, style)
    }
  }

  // placeholder - matches an image placeholder
  // e.g. `$1`
  def placeholder: Parser[IndexNode] = "$" ~ integer ^^ {
    case _ ~ index => IndexNode(index)
  }

  // source - matches either a url or image placeholder
  def source: Parser[ImageNode] = empty | url | placeholder

  // rgba - matches an rgba color with alpha
  // e.g. `rgba(0.5,0.5,0.5,1.0)`
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
  // e.g. `rgb(0.5,0.5,0.5)`
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
  // e.g. `rgb(0.5,0.5,0.5)50%`
  def colorStop: Parser[ColorStop] = color ~ percent ^^ {
    case color ~ number => ColorStop(color, number)
  }

  // linear gradient filter
  def linear: Parser[LinearGradientNode] = nameParser("linear(degrees,<csv list of colorStops>)")(
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

  // blur filter
  def blur: Parser[BlurNode] = nameParser("blur()")(
    "blur()" ^^ {
      case _ => BlurNode()
    })

  // box blur filter
  def boxblur: Parser[BoxBlurNode] = nameParser("boxblur(pixels,pixels)")(
    "boxblur(" ~> pixels ~ "," ~ pixels <~ ")" ^^ {
      case hRadius ~ _ ~ vRadius => BoxBlurNode(hRadius, vRadius)
    })

  def boxblurpercent: Parser[BoxBlurPercentNode] = nameParser("boxblur(percent,percent)")(
    "boxblur(" ~> percent ~ "," ~ percent <~ ")" ^^ {
      case hPercent ~ _ ~ vPercent => BoxBlurPercentNode(hPercent, vPercent)
    })

  // colorize filter
  def colorize: Parser[ColorizeNode] = nameParser("colorize(color)")(
    "colorize(" ~> color <~ ")" ^^ {
      case color => ColorizeNode(color)
    })

  // zoom filter
  def zoom: Parser[ZoomNode] = nameParser("zoom(percent)")(
    "zoom(" ~> percent <~ ")" ^^ {
      case percentage => ZoomNode(percentage)
    })

  // scale filter
  def scale: Parser[ScaleNode] = nameParser("scale(percent)")(
    "scale(" ~> percent <~ ")" ^^ {
      case percentage => ScaleNode(percentage)
    })

  // scaleto filter
  def scaleto: Parser[ScaleToNode] = nameParser("scaleto(pixels,pixels)")(
    "scaleto(" ~> pixels ~ "," ~ pixels <~ ")" ^^ {
      case width ~ _ ~ height => ScaleToNode(width, height)
    })

  // grid filter
  def grid: Parser[GridNode] = nameParser("grid(<csv list of urls>)")(
    "grid(" ~> repsep(source, ",") <~ ")" ^^ {
      case urls => GridNode(urls)
    })

  // text filter
  def text: Parser[TextNode] = nameParser("text(string,font,color)")(
    "text(" ~> string ~ "," ~ font ~ "," ~ color <~ ")" ^^ {
      case text ~ _ ~ font ~ _ ~ color => TextNode(text, font, color)
    })

  // textpercent filter
  def textpercent: Parser[TextPercentNode] = nameParser("text(string,fontpercent,color)")(
    "text(" ~> string ~ "," ~ fontpercent ~ "," ~ color <~ ")" ^^ {
      case text ~ _ ~ fontpercent ~ _ ~ color => TextPercentNode(text, fontpercent, color)
    })

  // round filter
  def round: Parser[RoundCornersNode] = nameParser("round(pixels)")(
    "round(" ~> pixels <~ ")" ^^ {
      case radius => RoundCornersNode(radius)
    })

  def roundpercent: Parser[RoundCornersPercentNode] = nameParser("round(percent)")(
    "round(" ~> percent <~ ")" ^^ {
      case percent => RoundCornersPercentNode(percent)
    })

  // mask filter
  def mask: Parser[MaskNode] = nameParser("mask(url,url)")(
    "mask(" ~> source ~ "," ~ source <~ ")" ^^ {
      case overlay ~ _ ~ mask => MaskNode(overlay, mask)
    })

  // overlay filter
  def overlay: Parser[OverlayNode] = nameParser("overlay(url)")(
    "overlay(" ~> source <~ ")" ^^ {
      case overlay => OverlayNode(overlay)
    })

  // pad filter
  def pad: Parser[PadNode] = nameParser("pad(pixels)")(
    "pad(" ~> pixels <~ ")" ^^ {
      case padding => PadNode(padding)
    })

  def padpercent: Parser[PadPercentNode] = nameParser("pad(percent)")(
    "pad(" ~> percent <~ ")" ^^ {
      case percent => PadPercentNode(percent)
    })

  // cover filter
  def cover: Parser[CoverNode] = nameParser("cover()")(
    "cover()" ^^ {
      case _ => CoverNode()
    })

  // fit filter
  def fit: Parser[FitNode] = nameParser("fit()")(
    "fit()" ^^ {
      case _ => FitNode()
    })

  // all filters
  def filters: Parser[FilterNode] =
    text | linear | boxblur | boxblurpercent |
    blur | scaleto | zoom | scale |
    grid | round | roundpercent | mask |
    cover | fit |
    colorize | overlay | pad | padpercent |
    textpercent

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
  def layer: Parser[LayerNode] =
    sourcelayer ||| sourcewithfilter ||| filterlayer

  // layers - matches all layers
  def layers: Parser[List[LayerNode]] = rep1sep(layer, ";")
}
