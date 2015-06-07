package com.rdio.thor

import java.awt.{Color, Font, GraphicsEnvironment}
import java.io.{File, FileInputStream}
import java.net.URL
import java.util.{Calendar, Date}

import scala.collection.mutable.ArrayBuffer

import com.sksamuel.scrimage.{Format, Image, ImageTools, ScaleMethod}
import com.sksamuel.scrimage.io.{ImageWriter, JpegWriter, PngWriter}
import com.sksamuel.scrimage.filter.{ColorizeFilter, BlurFilter}

import com.twitter.conversions.time._
import com.twitter.finagle.Service
import com.twitter.finagle.builder.ClientBuilder
import com.twitter.finagle.http.{Http, Status, RichHttp, Request, Response, Message}
import com.twitter.logging.Logger
import com.twitter.util.{Await, Future}
import com.typesafe.config.Config

import org.jboss.netty.handler.codec.http._
import org.jboss.netty.buffer.ChannelBuffers

/** ImageService serves images optionally filtered and blended. */
class ImageService(conf: Config, clients: Map[String, Service[Request, Response]]) extends BaseImageService(conf, clients) {

  protected def parserFactory(width: Int, height: Int) = new LayerParser(width, height)

  protected def requestFactory(
    layers: List[LayerNode],
    fetchedImages: Map[URL, Image],
    requestWidth: Option[Int],
    requestHeight: Option[Int]
  ) = new ImageRequest(layers, fetchedImages, requestWidth, requestHeight)

  // Ensures that any value is clamped between 1 and 1200
  def getDimension(dimension: Option[Int]) = dimension match {
    case Some(dimension) => Some(math.min(math.max(dimension, 1), 1200))
    case None => None
  }

  // Returns clamped values
  def getDimensions(w: Option[Int], h: Option[Int]): Tuple2[Option[Int], Option[Int]] =
    (getDimension(w), getDimension(h))

  // Extract all urls from layers
  def extractUrls(layers: List[LayerNode]): List[URL] = {
    layers flatMap {
      case LayerNode(url, GridNode(urls)) => url +: urls
      case LayerNode(url, MaskNode(overlay, mask)) => url +: List(overlay, mask)
      case LayerNode(url, OverlayNode(overlay)) => url +: List(overlay)
      case LayerNode(url, _: FilterNode) => List(url)
    } flatMap {
      case UrlNode(url) => List(url)
      case _ => List()
    }
  }

  // Build a map of paths to images (removing empty paths)
  def buildImageMap(paths: Array[URL], potentialImages: Array[Option[Image]]): Map[URL, Image] =
    (paths zip potentialImages).toMap.collect({
      case (path, Some(image)) => path -> image
    })

  // Converts a string format to a Format instance
  def getFormatter(format: Option[String]): Format[ImageWriter] = {
    format match {
      case Some("png") => Format.PNG.asInstanceOf[Format[ImageWriter]]
      case Some("gif") => Format.GIF.asInstanceOf[Format[ImageWriter]]
      case _ => Format.JPEG.asInstanceOf[Format[ImageWriter]]
    }
  }

  // Restrict compression to the range 0-100 (default 98)
  def getCompression(compression: Option[Int]): Int =
    math.min(math.max(compression.getOrElse(98), 0), 100)

  def apply(req: Request): Future[Response] = {
    req.params.get("l") match {
      case Some(layers) => {
        // Gather parameters
        val (width, height) = getDimensions(req.params.getInt("w"), req.params.getInt("h"))
        val compression = getCompression(req.params.getInt("c"))
        val format = getFormatter(req.params.get("f"))
        val parser = parserFactory(width.getOrElse(200), height.getOrElse(200))

        parser.parseAll(parser.layers, layers) match {
          case parser.Success(layers, _) => {
            val urls = extractUrls(layers).toArray

            requestImages(urls) map {
              potentialImages => {
                val fetchedImages = buildImageMap(urls, potentialImages.toArray)
                val request = requestFactory(layers, fetchedImages, width, height)

                request() match {
                  case Some(image) => buildResponse(req, image, format, compression)
                  case None => Response(HttpVersion.HTTP_1_1, HttpResponseStatus.NOT_FOUND)
                }
              }
            }
          }
          case unsuccess: parser.NoSuccess => {
            log.error(s"Failed to parse layers: $layers – ${unsuccess.toString}")

            val resp = Response(HttpVersion.HTTP_1_1, HttpResponseStatus.BAD_REQUEST)
            val parseErrPretty = unsuccess.toString.replace("^", """<span style="color: hsl(20, 100%, 50%); font-weight: 900;">^</span>""")
            resp.setHeader(HttpHeaders.Names.CONTENT_TYPE, "text/html")
            resp.write(
              s"""
              <p style="font-family: Helvetica; padding: 20px; margin-bottom: 2px; background-color: hsl(0, 0%, 80%)">Failed to parse layers: $layers</p>
              <pre style="padding: 32px; margin-top: 2px; background-color: hsl(0,0%,90%);"><code>${parseErrPretty}</code></pre>
              """)
            Future.value(resp)
          }
        }
      }
      case None => {
        log.error(s"No layers found in request: ${req.uri}")

        val resp = Response(HttpVersion.HTTP_1_1, HttpResponseStatus.BAD_REQUEST)
        resp.setHeader(HttpHeaders.Names.CONTENT_TYPE, "text/html")
        resp.write(s"<p>No layers found in request: ${req.uri}</p>")
        Future.value(resp)
      }
    }
  }
}
