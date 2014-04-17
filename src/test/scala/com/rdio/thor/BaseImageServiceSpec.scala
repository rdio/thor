package com.rdio.thor

import com.sksamuel.scrimage.{Format, Image}
import com.sksamuel.scrimage.io.{ImageWriter, JpegWriter, PngWriter}

class BaseImageServiceSpec extends BaseSpec {

  "getContentType" should "return the correct content-type per format" in {
    val pngFormat: Format[ImageWriter] = Format.PNG.asInstanceOf[Format[ImageWriter]]
    val jpegFormat: Format[ImageWriter] = Format.JPEG.asInstanceOf[Format[ImageWriter]]
    val gifFormat: Format[ImageWriter] = Format.GIF.asInstanceOf[Format[ImageWriter]]

    BaseImageService.getContentType(pngFormat) should be ("image/png")
    BaseImageService.getContentType(jpegFormat) should be ("image/jpeg")
    BaseImageService.getContentType(gifFormat) should be ("image/gif")
  }
}