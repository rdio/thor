package com.rdio.thor

import java.awt.{Color, Font}

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

  "MaskFilter" should "show overlay image with a white mask" in {
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

  "MaskFilter" should "blend overlay and original image by mask color" in {
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

  "MaskFilter" should "leave the mask layer untouched" in {
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
}