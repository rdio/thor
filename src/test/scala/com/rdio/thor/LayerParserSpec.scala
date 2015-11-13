package com.rdio.thor

import java.awt.{Color, Font}

class FilterParserSpec extends BaseSpec {

  val parser = new LayerParser(200, 200)

  "Parser" should "be able to parse numbers" in {
    parser.parseAll(parser.number, "10.0").get should be (10.0f)
  }

  it should "be able to parse percentages" in {
    parser.parseAll(parser.percent, "15%").get should be (0.15f)
  }

  it should "be able to parse degrees" in {
    parser.parseAll(parser.degrees, "45deg").get should be (45.0f)
  }

  it should "be able to parse booleans" in {
    parser.parseAll(parser.boolean, "true").get should be (true)
    parser.parseAll(parser.boolean, "false").get should be (false)
  }

  it should "be able to parse lengths" in {
    parser.parseAll(parser.length, "15%width").get should be (LengthPercentWidth(0.15f))
    parser.parseAll(parser.length, "15%height").get should be (LengthPercentHeight(0.15f))
    parser.parseAll(parser.length, "15%").get should be (LengthPercentage(0.15f))
    parser.parseAll(parser.length, "15px").get should be (LengthPixels(15))
  }

  it should "be able to parse colors" in {
    parser.parseAll(parser.rgba, "rgba(1, 0, 0, 1)").get should be (Color.red)
    parser.parseAll(parser.rgb, "rgb(0, 0, 1)").get should be (Color.blue)
    parser.parseAll(parser.color, "rgb(0, 1, 0)").get should be (Color.green)
    parser.parseAll(parser.color, "hsl(40, 0.5, 0.25)").get should be (new Color(96f/255f, 74f/255f, 32f/255f))
    parser.parseAll(parser.color, "hsla(40, 0.5, 0.25, 0.15)").get should be (new Color(96f/255f, 74f/255f, 32f/255f, 0.15f))
  }

  it should "be able to parse color stops" in {
    parser.parseAll(parser.colorStop, "rgb(0, 0, 1) 10%").get should be (ColorStop(Color.blue, 0.10f))
  }

  it should "be able to parse urls" in {
    parser.parseAll(parser.url, "http://localhost/album/9/3/0/0000000000003039/square-200.jpg").get should be {
      UrlNode("http://localhost/album/9/3/0/0000000000003039/square-200.jpg")
    }
  }

  it should "be able to parse paths" in {
    parser.parseAll(parser.url, "album/9/3/0/0000000000003039/square-200.jpg").get should be {
      UrlNode("album/9/3/0/0000000000003039/square-200.jpg")
    }
  }

  it should "be able to parse empty image placeholders" in {
    parser.parseAll(parser.empty, "_").get should be {
      EmptyNode()
    }
  }

  it should "be able to parse placeholders" in {
    parser.parseAll(parser.placeholder, "$1").get should be (IndexNode(1))
  }

  it should "be able to parse font styles" in {
    parser.parseAll(parser.fontStyle, "bold").get should be (1)
    parser.parseAll(parser.fontStyle, "italic").get should be (2)
    parser.parseAll(parser.fontStyle, "normal").get should be (0)
  }

  it should "be able to parse fonts" in {
    parser.parseAll(parser.font, "bold italic bold bold italic 16px \"Helvetica\"").get should be (FontPixelsNode("Helvetica", 16, 3))
    parser.parseAll(parser.font, "bold italic 16px \"Helvetica\"").get should be (FontPixelsNode("Helvetica", 16, 3))
    parser.parseAll(parser.font, "bold 16px \"Helvetica\"").get should be (FontPixelsNode("Helvetica", 16, 1))
    parser.parseAll(parser.font, "italic 16px \"Helvetica\"").get should be (FontPixelsNode("Helvetica", 16, 2))
    parser.parseAll(parser.font, "16px \"Helvetica\"").get should be (FontPixelsNode("Helvetica", 16, 0))
    parser.parseAll(parser.font, "\"Helvetica\"").get should be (FontPixelsNode("Helvetica", 12, 0))
    parser.parseAll(parser.font, "bold \"Helvetica\"").get should be (FontPixelsNode("Helvetica", 12, 1))
  }

  it should "be able to parse font percentages" in {
    parser.parseAll(parser.fontpercent, "bold italic bold bold italic 50% \"Helvetica\"").get should be (FontPercentNode("Helvetica", 0.5f, 3))
    parser.parseAll(parser.fontpercent, "bold italic 50% \"Helvetica\"").get should be (FontPercentNode("Helvetica", 0.5f, 3))
    parser.parseAll(parser.fontpercent, "bold 50% \"Helvetica\"").get should be (FontPercentNode("Helvetica", 0.5f, 1))
    parser.parseAll(parser.fontpercent, "italic 50% \"Helvetica\"").get should be (FontPercentNode("Helvetica", 0.5f, 2))
    parser.parseAll(parser.fontpercent, "50% \"Helvetica\"").get should be (FontPercentNode("Helvetica", 0.5f, 0))
    parser.parseAll(parser.fontpercent, "\"Helvetica\"").get should be (FontPercentNode("Helvetica", 1.0f, 0))
    parser.parseAll(parser.fontpercent, "bold \"Helvetica\"").get should be (FontPercentNode("Helvetica", 1.0f, 1))
  }

  it should "be able to parse text" in {
    parser.parseAll(parser.text, "text(\"Hello world!\", bold 16px \"Helvetica\", rgb(0, 0, 0))").get should be {
      TextNode("Hello world!", FontPixelsNode("Helvetica", 16, 1), Color.black)
    }
  }

  it should "be able to parse text percentages" in {
    parser.parseAll(parser.text, "text(\"Hello world!\", bold 50% \"Helvetica\", rgb(0, 0, 0))").get should be {
      TextNode("Hello world!", FontPercentNode("Helvetica", 0.5f, 1), Color.black)
    }
  }

  it should "be able to parse positioned text" in {
    parser.parseAll(parser.textOption, "bgColor=rgb(0,0,0)").get should be {
      TextOptions(Some(Color.black), None, None, None, None)
    }
    parser.parseAll(parser.textOptions, "options=[bgColor=rgb(0, 0, 0)]").get should be {
      TextOptions(Some(Color.black), None, None, None, None)
    }
    parser.parseAll(parser.textPositioned, "text(\"Hello world!\", bold 16px \"Helvetica\", rgb(0, 0, 0), centered, center, center, fitted(200px, 36px))").get should be {
      TextPositionedNode("Hello world!", FontPixelsNode("Helvetica", 16, 1), Color.black, List(Centered()), CenterAlign(), CenterAlign(), WidthFitted(200, LengthPixels(36)), TextOptions.empty)
    }
    parser.parseAll(parser.textPositioned, "text(\"Hello world!\", bold 25% \"Helvetica\", rgb(0, 0, 0), cartesian(25px, 25px), left, bottom, fitted(100px, 36px))").get should be {
      TextPositionedNode("Hello world!", FontPercentNode("Helvetica", 0.25f, 1), Color.black, List(CartesianAbsolute(25,25)), LeftAlign(), BottomAlign(), WidthFitted(100, LengthPixels(36)), TextOptions.empty)
    }
    parser.parseAll(parser.textPositioned, "text(\"Hello world!\", bold 25% \"Helvetica\", rgb(0, 0, 0), cartesian(25px, 25px), left, bottom, fitted(100px, 36px), options=[bgColor=rgb(0, 0, 0)])").get should be {
      TextPositionedNode("Hello world!", FontPercentNode("Helvetica", 0.25f, 1), Color.black, List(CartesianAbsolute(25,25)), LeftAlign(), BottomAlign(), WidthFitted(100, LengthPixels(36)), TextOptions(Some(Color.black), None, None, None, None))
    }
    parser.parseAll(parser.textPositioned, "text(\"Hello world!\", bold 25% \"Helvetica\", rgb(0, 0, 0), [cartesian(50%, 61.8%), cartesian(25px, 10px)], right, top, fromContent)").get should be {
      TextPositionedNode("Hello world!", FontPercentNode("Helvetica", 0.25f, 1), Color.black, List(CartesianRelative(0.5f,0.618f), CartesianAbsolute(25,10)), RightAlign(), TopAlign(), WidthFromContent(), TextOptions.empty)
    }
  }

  it should "be able to parse filters" in {
    parser.parseAll(parser.linear, "linear(45deg, rgb(1, 0, 0) 100%, rgb(0, 0, 1) 0%)").get should be {
      LinearGradientNode(45, List(Color.blue, Color.red), List(0.0f, 1.0f))
    }
    parser.parseAll(parser.blur, "blur()").get should be (BlurNode())
    parser.parseAll(parser.boxblur, "boxblur(10px, 10px)").get should be (BoxBlurNode(10, 10))
    parser.parseAll(parser.colorize, "colorize(rgba(0, 0, 0, 0.2))").get should be (ColorizeNode(new Color(0f, 0f, 0f, 0.2f)))
    parser.parseAll(parser.scale, "scale(10%)").get should be (ScaleNode(0.1f))
    parser.parseAll(parser.grid, "grid(a.png, b.png, c.png, d.png)").get should be {
      GridNode(List(UrlNode("a.png"), UrlNode("b.png"), UrlNode("c.png"), UrlNode("d.png")))
    }
    parser.parseAll(parser.round, "round(10px)").get should be (RoundCornersNode(10))
    parser.parseAll(parser.mask, "mask(overlay.png, http://localhost/mask.png)").get should be {
      MaskNode(UrlNode("overlay.png"), UrlNode("http://localhost/mask.png"))
    }
    parser.parseAll(parser.frame, "frame(12px, rgba(0, 0, 0, 0.2))").get should be (FrameNode(LengthPixels(12), new Color(0f, 0f, 0f, 0.2f)))
    parser.parseAll(parser.frame, "frame(9%, rgba(0, 0, 0, 0.2))").get should be (FrameNode(LengthPercentage(0.09f), new Color(0f, 0f, 0f, 0.2f)))
  }

  it should "be able to parse a layer" in {
    parser.parseAll(parser.layer, "album/0/1/2/a.png").get should be (LayerNode(UrlNode("album/0/1/2/a.png"), NoopNode()))
    parser.parseAll(parser.layer, "album/0/3/4/b.png:blur()").get should be (LayerNode(UrlNode("album/0/3/4/b.png"), BlurNode()))
    parser.parseAll(parser.layer, "$1:blur()").get should be (LayerNode(IndexNode(1), BlurNode()))
  }

  it should "be able to parse multiple layers" in {
    parser.parseAll(parser.layers, "album/0/1/2/a.png;$0:blur()").get should be {
      List(
        LayerNode(UrlNode("album/0/1/2/a.png"), NoopNode()),
        LayerNode(IndexNode(0), BlurNode())
      )
    }
  }
}