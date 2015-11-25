package com.rdio.thor

import java.awt.{Color, Font, Graphics2D}
import com.sksamuel.scrimage.{Image, Pixel}

class ImageFiltersSpec extends BaseSpec {

  val parser = new LayerParser(200, 200)

  "LinearGradientFilter" should "respect color but not alpha" in {
    val colors:Array[Color] = Array(new Color(255, 0, 0, 0), new Color(0, 255, 0, 255))
    val stops:Array[Float] = Array(0.05f, 0.9f)

    val filter = new LinearGradientFilter(0, colors, stops)
    val img = Image.filled(10, 10, new Color(0, 0, 255, 100))
    filter(img)
    val pixel0 = img.pixels(0)
    val pixel50 = img.pixels(45)
    val pixel99 = img.pixels(99)

    // shows the base image
    pixel0.red should be (0)
    pixel0.green should be (0)
    pixel0.blue should be (255)
    pixel0.alpha should be (100)

    // mid transition
    pixel50.red should be (96)
    pixel50.green should be (67)
    pixel50.blue should be (92)
    pixel50.alpha should be (164)

    // shows the end of the gradiant
    pixel99.red should be (0)
    pixel99.green should be (255)
    pixel99.blue should be (0)
    pixel99.alpha should be (255)
  }

  "MaskFilter" should "show the original image with a black mask" in {
    val img = Image.filled(5, 5, new Color(0, 0, 255, 255))
    val overlay = Image.filled(5, 5, new Color(255, 0, 0, 255))
    var mask = Image.filled(5, 5, Color.BLACK)
    val filter = new MaskFilter(overlay: Image, mask: Image)
    filter(img)

    var pixel = img.pixels(0)

    pixel.red should be (0)
    pixel.green should be (0)
    pixel.blue should be (255)
    pixel.alpha should be (255)
  }

  it should "show overlay image with a white mask" in {
    val img = Image.filled(5, 5, new Color(0, 0, 255, 255))
    val overlay = Image.filled(5, 5, new Color(255, 0, 0, 255))
    var mask = Image.filled(5, 5, Color.WHITE)
    val filter = new MaskFilter(overlay: Image, mask: Image)
    filter(img)

    var pixel = img.pixels(0)

    pixel.red should be (255)
    pixel.green should be (0)
    pixel.blue should be (0)
    pixel.alpha should be (255)
  }

  it should "blend overlay and original image by mask color" in {
    val img = Image.filled(5, 5, new Color(0, 0, 255, 255))
    val overlay = Image.filled(5, 5, new Color(255, 0, 0, 255))
    var mask = Image.filled(5, 5, Color.BLUE)
    val filter = new MaskFilter(overlay: Image, mask: Image)
    filter(img)

    var pixel = img.pixels(0)

    pixel.red should be (28)
    pixel.green should be (0)
    pixel.blue should be (227)
    pixel.alpha should be (255)
  }

  it should "leave the mask layer untouched" in {
    val img = Image.filled(5, 5, new Color(0, 0, 255, 255))
    val overlay = Image.filled(5, 5, new Color(255, 0, 0, 255))
    var mask = Image.filled(5, 5, new Color(27, 200, 93, 255))
    val filter = new MaskFilter(overlay: Image, mask: Image)
    filter(img)

    var pixel = mask.pixels(0)

    pixel.red should be (27)
    pixel.green should be (200)
    pixel.blue should be (93)
    pixel.alpha should be (255)
  }

  trait FrameFilterFixture {
    val width  = 300
    val height = 400
    val thickness = 3
    val imageColor = com.sksamuel.scrimage.Color.White
    val frameColor = com.sksamuel.scrimage.Color.Black
    val imagePixel = Pixel(imageColor)
    val framePixel = Pixel(frameColor)
    val blank = Image.filled(width, height, imageColor)
  }

  "FrameFilter" should "fill in the frame regions (thickness specified in pixels)" in {
    new FrameFilterFixture {
      val framed = blank.filter(FrameFilter(LengthPixels(3), frameColor))
      framed.pixel(0, 0) should be (framePixel)
      framed.pixel(width-1, 0) should be (framePixel)
      framed.pixel(0, height-1) should be (framePixel)
      framed.pixel(width-1, height-1) should be (framePixel)

      framed.pixel(thickness,   0)           should be (framePixel)
      framed.pixel(thickness,   thickness-1) should be (framePixel)
      framed.pixel(0,           thickness)   should be (framePixel)
      framed.pixel(thickness-1, thickness)   should be (framePixel)
      framed.pixel(thickness-1, thickness-1) should be (framePixel)

      framed.pixel(width-thickness-1, 0)           should be (framePixel)
      framed.pixel(width-thickness-1, thickness-1) should be (framePixel)
      framed.pixel(width-thickness,   thickness)   should be (framePixel)
      framed.pixel(width-1,           thickness)   should be (framePixel)
      framed.pixel(width-thickness,   thickness-1) should be (framePixel)

      framed.pixel(0,           height-thickness-1) should be (framePixel)
      framed.pixel(thickness-1, height-thickness-1) should be (framePixel)
      framed.pixel(thickness,   height-thickness)   should be (framePixel)
      framed.pixel(thickness,   height-1)           should be (framePixel)
      framed.pixel(thickness-1, height-thickness)   should be (framePixel)

      framed.pixel(width-thickness-1, height-thickness)   should be (framePixel)
      framed.pixel(width-thickness-1, height-1)           should be (framePixel)
      framed.pixel(width-thickness,   height-thickness-1) should be (framePixel)
      framed.pixel(width-1,           height-thickness-1) should be (framePixel)
      framed.pixel(width-thickness,   height-thickness)   should be (framePixel)
    }
  }

  it should "leave the interior of the frame intact (thickness specified in pixels)" in {
    new FrameFilterFixture {
      val framed = blank.filter(FrameFilter(LengthPixels(3), frameColor))
      framed.pixel(thickness,         thickness)          should be (imagePixel)
      framed.pixel(width-thickness-1, thickness)          should be (imagePixel)
      framed.pixel(thickness,         height-thickness-1) should be (imagePixel)
      framed.pixel(width-thickness-1, height-thickness-1) should be (imagePixel)
    }
  }

  it should "fill in the frame regions (thickness specified in percentage)" in {
    new FrameFilterFixture {
      val percentageThickness = 0.05f
      val framed = blank.filter(FrameFilter(LengthPercentage(percentageThickness), frameColor))
      val hThickness = (width * percentageThickness).toInt
      val vThickness = (height * percentageThickness).toInt
      framed.pixel(0, 0) should be (framePixel)
      framed.pixel(width-1, 0) should be (framePixel)
      framed.pixel(0, height-1) should be (framePixel)
      framed.pixel(width-1, height-1) should be (framePixel)

      framed.pixel(hThickness,   0)           should be (framePixel)
      framed.pixel(hThickness,   vThickness-1) should be (framePixel)
      framed.pixel(0,            vThickness)   should be (framePixel)
      framed.pixel(hThickness-1, vThickness)   should be (framePixel)
      framed.pixel(hThickness-1, vThickness-1) should be (framePixel)

      framed.pixel(width-hThickness-1, 0)           should be (framePixel)
      framed.pixel(width-hThickness-1, vThickness-1) should be (framePixel)
      framed.pixel(width-hThickness,   vThickness)   should be (framePixel)
      framed.pixel(width-1,            vThickness)   should be (framePixel)
      framed.pixel(width-hThickness,   vThickness-1) should be (framePixel)

      framed.pixel(0,            height-vThickness-1) should be (framePixel)
      framed.pixel(hThickness-1, height-vThickness-1) should be (framePixel)
      framed.pixel(hThickness,   height-vThickness)   should be (framePixel)
      framed.pixel(hThickness,   height-1)           should be (framePixel)
      framed.pixel(hThickness-1, height-vThickness)   should be (framePixel)

      framed.pixel(width-hThickness-1, height-vThickness)   should be (framePixel)
      framed.pixel(width-hThickness-1, height-1)           should be (framePixel)
      framed.pixel(width-hThickness,   height-vThickness-1) should be (framePixel)
      framed.pixel(width-1,            height-vThickness-1) should be (framePixel)
      framed.pixel(width-hThickness,   height-vThickness)   should be (framePixel)
    }
  }

  it should "leave the interior of the frame intact (thickness specified in percentage)" in {
    new FrameFilterFixture {
      val percentageThickness = 0.05f
      val framed = blank.filter(FrameFilter(LengthPercentage(percentageThickness), frameColor))
      val hThickness = (width * percentageThickness).toInt
      val vThickness = (height * percentageThickness).toInt
      framed.pixel(hThickness,         vThickness)          should be (imagePixel)
      framed.pixel(width-hThickness-1, vThickness)          should be (imagePixel)
      framed.pixel(hThickness,         height-vThickness-1) should be (imagePixel)
      framed.pixel(width-hThickness-1, height-vThickness-1) should be (imagePixel)
    }
  }

  it should "fill in the frame regions (thickness specified in percentage-width)" in {
    new FrameFilterFixture {
      val percentageThickness = 0.05f
      val framed = blank.filter(FrameFilter(LengthPercentWidth(percentageThickness), frameColor))
      override val thickness = (width * percentageThickness).toInt
      framed.pixel(0, 0) should be (framePixel)
      framed.pixel(width-1, 0) should be (framePixel)
      framed.pixel(0, height-1) should be (framePixel)
      framed.pixel(width-1, height-1) should be (framePixel)

      framed.pixel(thickness,   0)           should be (framePixel)
      framed.pixel(thickness,   thickness-1) should be (framePixel)      
      framed.pixel(0,           thickness)   should be (framePixel)
      framed.pixel(thickness-1, thickness)   should be (framePixel)
      framed.pixel(thickness-1, thickness-1) should be (framePixel)      

      framed.pixel(width-thickness-1, 0)           should be (framePixel)
      framed.pixel(width-thickness-1, thickness-1) should be (framePixel)
      framed.pixel(width-thickness,   thickness)   should be (framePixel)
      framed.pixel(width-1,           thickness)   should be (framePixel)
      framed.pixel(width-thickness,   thickness-1) should be (framePixel)

      framed.pixel(0,           height-thickness-1) should be (framePixel)
      framed.pixel(thickness-1, height-thickness-1) should be (framePixel)
      framed.pixel(thickness,   height-thickness)   should be (framePixel)
      framed.pixel(thickness,   height-1)           should be (framePixel)
      framed.pixel(thickness-1, height-thickness)   should be (framePixel)

      framed.pixel(width-thickness-1, height-thickness)   should be (framePixel)
      framed.pixel(width-thickness-1, height-1)           should be (framePixel)
      framed.pixel(width-thickness,   height-thickness-1) should be (framePixel)
      framed.pixel(width-1,           height-thickness-1) should be (framePixel)
      framed.pixel(width-thickness,   height-thickness)   should be (framePixel)
    }
  }

  it should "leave the interior of the frame intact (thickness specified in percentage-width)" in {
    new FrameFilterFixture {
      val percentageThickness = 0.05f
      val framed = blank.filter(FrameFilter(LengthPercentWidth(percentageThickness), frameColor))
      override val thickness = (width * percentageThickness).toInt
      framed.pixel(thickness,         thickness)          should be (imagePixel)
      framed.pixel(width-thickness-1, thickness)          should be (imagePixel)
      framed.pixel(thickness,         height-thickness-1) should be (imagePixel)
      framed.pixel(width-thickness-1, height-thickness-1) should be (imagePixel)
    }
  }

  // Helpers for testing TextFilter

  def imageHasStuffInRegion(img: Image, baselineColor: Color, x: Int, y: Int, w: Int, h: Int): Boolean = {
    val region = img.subimage(x, y, w, h)
    val basePixel = Pixel(baselineColor)
    !(region.pixels.forall(x => x == basePixel))
  }

  def imageDoesntHaveStuffOutsideOfRegion(img: Image, baselineColor: Color, x: Int, y: Int, w: Int, h: Int): Boolean = {
    val image = img.copy
    val g2 = image.awt.getGraphics.asInstanceOf[Graphics2D]
    g2.setColor(baselineColor)
    g2.fillRect(x, y, w, h)
    g2.dispose()

    val basePixel = Pixel(baselineColor)
    image.pixels.forall(x => x == basePixel)
  }

  def imageOnlyHasStuffInRegion(img: Image, baselineColor: Color, x: Int, y: Int, w: Int, h: Int): Boolean = {
    imageHasStuffInRegion(img, baselineColor, x, y, w, h) && imageDoesntHaveStuffOutsideOfRegion(img, baselineColor, x, y, w, h)
  }

  trait TextUtilsFixture {
    val font = new Font("Helvetica", Font.PLAIN, 12)
    val imageColor = com.sksamuel.scrimage.Color.White
    val textColor = com.sksamuel.scrimage.Color.Black
    val textBackgroundColor = com.sksamuel.scrimage.Color(255,192,192)
    val width  = 600
    val height = 80
    val blank = Image.filled(width, height, imageColor)
    val g2 = blank.awt.getGraphics.asInstanceOf[Graphics2D]
    val lineHeightMultiplier = 1.382f
    val maxFontSize = 36
    val text = "zum schreiben, auf zeichnung"

  }

  "TextUtils" should "create line breaks correctly" in {
    new TextUtilsFixture {
      TextUtils.breakTextIntoLines(font, g2, text, 600, 80, lineHeightMultiplier, maxFontSize) shouldEqual((36, List((text, 478))))
      TextUtils.breakTextIntoLines(font, g2, text, 400, 80, lineHeightMultiplier, maxFontSize) shouldEqual((29, List((text, 385))))
      TextUtils.breakTextIntoLines(font, g2, text, 240, 80, lineHeightMultiplier, maxFontSize) shouldEqual((28, List(("zum schreiben,", 192), ("auf zeichnung", 178))))
    }
  }

  it should "layout text in the correct positions" in {
    new TextUtilsFixture {
      val (fontSize, lines) = TextUtils.breakTextIntoLines(font, g2, text, 240, 80, lineHeightMultiplier, maxFontSize)

      TextUtils.layoutText(g2, 300, 200, CenterAlign(), CenterAlign(), lines, fontSize, lineHeightMultiplier) shouldEqual(204, 161, 192, 78, List(("zum schreiben,",204,180), ("auf zeichnung",211,219)))
      TextUtils.layoutText(g2, 300, 200, CenterAlign(), TopAlign(), lines, fontSize, lineHeightMultiplier) shouldEqual(204, 200, 192, 78, List(("zum schreiben,",204,200), ("auf zeichnung",211,239)))
      TextUtils.layoutText(g2, 300, 200, CenterAlign(), BottomAlign(), lines, fontSize, lineHeightMultiplier) shouldEqual(204, 122, 192, 78, List(("zum schreiben,",204,161), ("auf zeichnung",211,200)))
      TextUtils.layoutText(g2, 300, 200, LeftAlign(), CenterAlign(), lines, fontSize, lineHeightMultiplier) shouldEqual(300, 161, 192, 78, List(("zum schreiben,",300,180), ("auf zeichnung",300,219)))
      TextUtils.layoutText(g2, 300, 200, RightAlign(), CenterAlign(), lines, fontSize, lineHeightMultiplier) shouldEqual(108, 161, 192, 78, List(("zum schreiben,",108,180), ("auf zeichnung",122,219)))
    }
  }

  it should "place the background rect in the expected position" in {
    new TextUtilsFixture {
      val (fontSize, lines) = TextUtils.breakTextIntoLines(font, g2, text, 240, 80, lineHeightMultiplier, maxFontSize)
      val (overallX, overallY, overallWidth, overallHeight, linesWithPositions) =
        TextUtils.layoutText(g2, 300, 200, RightAlign(), CenterAlign(), lines, fontSize, lineHeightMultiplier)
      val textOptions =
        TextOptions(None,
          Some(LengthPercentage(0.25f)), // CSS ordering is top, right, bottom, left
          Some(LengthPercentage(0.25f)),
          Some(LengthPercentage(0.25f)),
          Some(LengthPercentage(0.25f)))

      TextUtils.getBackgroundRect(blank, overallX, overallY, overallWidth, overallHeight, fontSize, lineHeightMultiplier, textOptions) shouldEqual ((101, 116, 206, 92))
    }
  }

  trait TextFilterFixture {
    val font = new Font("Helvetica", Font.PLAIN, 12)

    // these are really strict bounds
    val expectedTextWidth = 40
    val expectedTextHeight = 12

    val width  = 300
    val height = 400
    val imageColor = com.sksamuel.scrimage.Color.White
    val textColor = com.sksamuel.scrimage.Color.Black
    val imagePixel = Pixel(imageColor)
  }

  "TextFilter" should "render text in the correct place (centered position, center horizontal alignment, center vertical alignment)" in {
    new TextFilterFixture {
      val expectedX = width/2 - expectedTextWidth/2
      val expectedY = height/2 - expectedTextHeight/2
      val blank = Image.filled(width, height, imageColor)
      val withText = blank.filter(TextFilter("Testing", font, textColor))

      imageOnlyHasStuffInRegion(withText, imageColor, expectedX, expectedY, expectedTextWidth, expectedTextHeight)
    }
  }

  it should "render text in the correct place (relative position at (0.5, 0.25), center horizontal alignment, top vertical alignment)" in {
    new TextFilterFixture {
      val posX = (0.5).toFloat
      val posY = (0.25).toFloat
      val blank = Image.filled(width, height, imageColor)
      val withText = blank.filter(TextFilter("Testing", font, textColor, List(CartesianRelative(posX, posY)), CenterAlign(), TopAlign()))
  
      val expectedX = (width * posX) - (expectedTextWidth / 2)
      val expectedY = height * posY

      imageOnlyHasStuffInRegion(withText, imageColor, expectedX.toInt, expectedY.toInt, expectedTextWidth, expectedTextHeight)
    }
  }

  it should "render text in the correct place (relative position at (0.5, 0.25), center horizontal alignment, bottom vertical alignment)" in {
    new TextFilterFixture {
      val posX = (0.5).toFloat
      val posY = (0.25).toFloat
      val blank = Image.filled(width, height, imageColor)
      val withText = blank.filter(TextFilter("Testing", font, textColor, List(CartesianRelative(posX, posY)), CenterAlign(), BottomAlign()))
  
      val expectedX = (width * posX) - (expectedTextWidth / 2)
      val expectedY = height * posY - expectedTextHeight

      imageOnlyHasStuffInRegion(withText, imageColor, expectedX.toInt, expectedY.toInt, expectedTextWidth, expectedTextHeight)
    }
  }

  it should "be able to fit text to a given width (shrink text to fit)" in {
    new TextFilterFixture {
      val posX = 10
      val posY = 10
      val testString = "abjsel kfsjdf kldsjk flsdjf lkjouh"
      val blank = Image.filled(width, height, imageColor)
      override val expectedTextWidth = 200
      override val expectedTextHeight = 36
      val withText = blank.filter(TextFilter(testString, font, textColor, List(CartesianAbsolute(posX, posY)), LeftAlign(), TopAlign(), WidthFitted(LengthPixels(expectedTextWidth), LengthPixels(expectedTextHeight))))

      imageOnlyHasStuffInRegion(withText, imageColor, posX, posY, expectedTextWidth, expectedTextHeight)
    }
  }

  it should "be able to fit text to a given width (don't exceed maximum font size)" in {
    new TextFilterFixture {
      val posX = 10
      val posY = 10
      val testString = "Oh Hai!"
      val blank = Image.filled(width, height, imageColor)
      override val expectedTextWidth = 200
      override val expectedTextHeight = 36
      val withText = blank.filter(TextFilter(testString, font, textColor, List(CartesianAbsolute(posX, posY)), LeftAlign(), TopAlign(), WidthFitted(LengthPixels(expectedTextWidth), LengthPixels(expectedTextHeight))))

      imageOnlyHasStuffInRegion(withText, imageColor, posX, posY, expectedTextWidth, expectedTextHeight)
    }
  }

  it should "be able to fit text to a given width and height (don't exceed maximum font size)" in {
    new TextFilterFixture {
      override val expectedTextWidth = 206
      override val expectedTextHeight = 92

      override val width  = 600
      override val height = 400
      val blank = Image.filled(width, height, imageColor)

      val expectedFontSize = 28
      val testString = "zum schreiben, auf zeichnung"
      val textOptions =
        TextOptions(Some(textColor),
          Some(LengthPercentage(0.25f)), // CSS ordering is top, right, bottom, left
          Some(LengthPercentage(0.25f)),
          Some(LengthPercentage(0.25f)),
          Some(LengthPercentage(0.25f)))

      val withText =
        blank.filter(TextFilter(testString, font, textColor, List(CartesianRelative(0.5f, 0.5f)), RightAlign(), CenterAlign(),
          WidthAndHeightFitted(LengthPixels(240), LengthPixels(80), LengthPixels(expectedFontSize)), textOptions))

      imageOnlyHasStuffInRegion(withText, imageColor, 300, 200, expectedTextWidth, expectedTextHeight)
    }
  }
}
