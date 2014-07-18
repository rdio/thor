package com.rdio.thor

import com.sksamuel.scrimage.{Image, ScaleMethod, Format}
import com.sksamuel.scrimage.io.{ImageWriter, JpegWriter, PngWriter}

package object extensions {

  implicit class ImageExtensions(image: Image) {
    def condScaleTo(width: Int, height: Int, method: ScaleMethod = ScaleMethod.Bicubic): Image = {
      if (image.width != width || image.height != height) {
        if (width < 3 || height < 3) {
          // Cannot use bicubic scaling on small images (errors)
          image.scaleTo(width, height, ScaleMethod.FastScale)
        } else {
          image.scaleTo(width, height, method)
        }
      } else {
        image
      }
    }

    def aspectRatio: Float = image.width.toFloat / image.height.toFloat
  }
}
