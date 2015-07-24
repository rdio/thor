package com.rdio.thor

import com.sksamuel.scrimage.{Format, Image}

class BaseImageServiceSpec extends BaseSpec {

  "getContentType" should "return the correct content-type per format" in {
    val pngFormat: Format = Format.PNG
    val jpegFormat: Format = Format.JPEG
    val gifFormat: Format = Format.GIF

    BaseImageService.getContentType(pngFormat) should be ("image/png")
    BaseImageService.getContentType(jpegFormat) should be ("image/jpeg")
    BaseImageService.getContentType(gifFormat) should be ("image/gif")
  }
}