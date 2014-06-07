package com.rdio.thor

import com.sksamuel.scrimage.Image
import com.sksamuel.scrimage.io.ImageWriter
import com.twitter.logging.Logger
import java.io.OutputStream
import java.awt.image.BufferedImage
import javax.imageio.{ IIOImage, ImageTypeSpecifier, ImageWriteParam, ImageIO }
import javax.imageio.metadata.IIOMetadataNode
import org.apache.commons.io.IOUtils
import javax.imageio.stream.MemoryCacheImageOutputStream

case class Frame(image: Image, durationInMs: Int)
class AnimatedGifWriter(frames: List[Frame]) extends ImageWriter {
  

  protected lazy val log = Logger.get(this.getClass)

  def write(out: OutputStream) {
    val writer = ImageIO.getImageWritersByFormatName("gif").next()
    val params = writer.getDefaultWriteParam
    // TODO : OH SHIT!! What is our BufferedImage type?!?!

    val metaData = writer.getDefaultImageMetadata(ImageTypeSpecifier.createFromBufferedImageType(BufferedImage.TYPE_INT_ARGB), params)
    val root:IIOMetadataNode = new IIOMetadataNode(metaData.getNativeMetadataFormatName())

    val graphicsControlExtensionNode:IIOMetadataNode = new IIOMetadataNode("GraphicControlExtension")
    graphicsControlExtensionNode.setAttribute("disposalMethod", "none")
    graphicsControlExtensionNode.setAttribute("userInputFlag", "FALSE")
    graphicsControlExtensionNode.setAttribute("transparentColorFlag", "FALSE")
    graphicsControlExtensionNode.setAttribute("delayTime", (10).toString) // stupid! "Delay Time (1/100ths of a second)"
    graphicsControlExtensionNode.setAttribute("transparentColorIndex", "0")  // somewhere else has it as 255
    root.appendChild(graphicsControlExtensionNode)

    val appEntensionsNode:IIOMetadataNode = new IIOMetadataNode("ApplicationExtensions")
    val child:IIOMetadataNode = new IIOMetadataNode("ApplicationExtension")
    child.setAttribute("applicationID", "NETSCAPE")
    child.setAttribute("authenticationCode", "2.0")
    val byteArray:Array[Byte] = Array(0x01, 0x00, 0x00)  //  The last two bytes is the unsigned short (little endian) that represents the number of times to loop. 0 means loop forever. 
    child.setUserObject(byteArray)
    appEntensionsNode.appendChild(child)
    root.appendChild(appEntensionsNode)

    metaData.mergeTree(metaData.getNativeMetadataFormatName(), root)

    val output = new MemoryCacheImageOutputStream(out)
    writer.setOutput(output)
    writer.prepareWriteSequence(null)
    frames.zipWithIndex foreach {
      case (frame:Frame, i) => {
        val count = frame.durationInMs
        graphicsControlExtensionNode.setAttribute("delayTime", (count / 10).toString) // stupid! "Delay Time (1/100ths of a second)"
        root.appendChild(graphicsControlExtensionNode)
        metaData.mergeTree(metaData.getNativeMetadataFormatName(), root)
        writer.writeToSequence(new IIOImage(frame.image.awt, null, metaData), params)
      }
    }
    writer.endWriteSequence()
    writer.dispose()
    output.close()
    IOUtils.closeQuietly(out)
  }
}

object AnimatedGifWriter {
  def apply(frames: List[Frame]): AnimatedGifWriter = new AnimatedGifWriter(frames)
}