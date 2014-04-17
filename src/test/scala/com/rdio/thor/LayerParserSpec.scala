package com.rdio.thor

import java.awt.Color

class FilterParserSpec extends BaseSpec {

  val parser = new LayerParser(200, 200)

  "Parser" should "be able to parse numbers" in {
    parser.parseAll(parser.number, "10.0").get should be (10.0f)
  }

  "Parser" should "be able to parse percentages" in {
    parser.parseAll(parser.percent, "15%").get should be (0.15f)
  }

  "Parser" should "be able to parse degrees" in {
    parser.parseAll(parser.degrees, "45deg").get should be (45.0f)
  }

  "Parser" should "be able to parse colors" in {
    parser.parseAll(parser.rgba, "rgba(1, 0, 0, 1)").get should be (Color.red)
    parser.parseAll(parser.rgb, "rgb(0, 0, 1)").get should be (Color.blue)
    parser.parseAll(parser.color, "rgb(0, 1, 0)").get should be (Color.green)
  }

  "Parser" should "be able to parse color stops" in {
    parser.parseAll(parser.colorStop, "rgb(0, 0, 1) 10%").get should be (ColorStop(Color.blue, 0.10f))
  }

  "Parser" should "be able to parse paths" in {
    parser.parseAll(parser.path, "album/9/3/0/0000000000003039/square-200.jpg").get should be {
      PathNode("album/9/3/0/0000000000003039/square-200.jpg")
    }
  }

  "Parser" should "be able to parse empty image placeholders" in {
    parser.parseAll(parser.empty, "_").get should be {
      EmptyNode()
    }
  }

  "Parser" should "be able to parse placeholders" in {
    parser.parseAll(parser.placeholder, "$1").get should be (IndexNode(1))
  }

  "Parser" should "be able to parse filters" in {
    parser.parseAll(parser.linear, "linear(45deg, rgb(1, 0, 0) 100%, rgb(0, 0, 1) 0%)").get should be {
      LinearGradientNode(45, List(Color.blue, Color.red), List(0.0f, 1.0f))
    }
    parser.parseAll(parser.blur, "blur()").get should be (BlurNode())
    parser.parseAll(parser.boxblur, "boxblur(10px, 10px)").get should be (BoxBlurNode(10, 10))
    parser.parseAll(parser.colorize, "colorize(rgba(0, 0, 0, 0.2))").get should be (ColorizeNode(new Color(0f, 0f, 0f, 0.2f)))
    parser.parseAll(parser.scale, "scale(10%)").get should be (ScaleNode(0.1f))
    parser.parseAll(parser.grid, "grid(a.png, b.png, c.png, d.png)").get should be {
      GridNode(List(PathNode("a.png"), PathNode("b.png"), PathNode("c.png"), PathNode("d.png")))
    }
    parser.parseAll(parser.round, "round(10px)").get should be (RoundCornersNode(10))
    parser.parseAll(parser.mask, "mask(overlay.png, mask.png)").get should be {
      MaskNode(PathNode("overlay.png"), PathNode("mask.png"))
    }
  }

  "Parser" should "be able to parse a layer" in {
    parser.parseAll(parser.layer, "album/0/1/2/a.png").get should be (LayerNode(PathNode("album/0/1/2/a.png"), NoopNode()))
    parser.parseAll(parser.layer, "album/0/3/4/b.png:blur()").get should be (LayerNode(PathNode("album/0/3/4/b.png"), BlurNode()))
    parser.parseAll(parser.layer, "$1:blur()").get should be (LayerNode(IndexNode(1), BlurNode()))
  }

  "Parser" should "be able to parse multiple layers" in {
    parser.parseAll(parser.layers, "album/0/1/2/a.png;$0:blur()").get should be {
      List(
        LayerNode(PathNode("album/0/1/2/a.png"), NoopNode()),
        LayerNode(IndexNode(0), BlurNode())
      )
    }
  }
}