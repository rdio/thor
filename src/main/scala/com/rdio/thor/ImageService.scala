package com.rdio.thor

import java.awt.{Color, Font, GraphicsEnvironment}
import java.io.{File, FileInputStream}
import java.net.InetSocketAddress
import java.util.{Calendar, Date}

import scala.collection.mutable.ArrayBuffer

import com.sksamuel.scrimage.{Format, Image, ScaleMethod}
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
class ImageService(conf: Config, client: Service[Request, Response]) extends BaseImageService(conf, client) {

  protected def parserFactory(width: Int, height: Int) = new LayerParser(width, height)

  protected def requestFactory(
    layers: List[LayerNode],
    fetchedImages: Map[String, Image],
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

  // Extract all paths from layers
  def extractPaths(layers: List[LayerNode]): List[String] = {
    layers flatMap {
      case LayerNode(path, GridNode(paths)) => path +: paths
      case LayerNode(path, MaskNode(overlay, mask)) => path +: List(overlay, mask)
      case LayerNode(path, OverlayNode(overlay)) => path +: List(overlay)
      case LayerNode(path, _: FilterNode) => List(path)
    } flatMap {
      case PathNode(path) => List(path)
      case _ => List()
    }
  }

  // Build a map of paths to images (removing empty paths)
  def buildImageMap(paths: Array[String], potentialImages: Array[Option[Image]]): Map[String, Image] =
    (paths zip potentialImages).toMap.collect({
      case (path, Some(image)) => path -> image
    })

  // Converts a string format to a Format instance
  def getFormatter(format: Option[String]): Format = {
    format match {
      case Some("png") => Format.PNG
      case Some("gif") => Format.GIF
      case _ => Format.JPEG
    }
  }

  // Restrict compression to the range 0-100 (default 98)
  def getCompression(compression: Option[Int]): Int =
    math.min(math.max(compression.getOrElse(98), 0), 100)

  def handleHelp(req: Request): Future[Response] = {
    val resp = Response(HttpVersion.HTTP_1_1, HttpResponseStatus.OK)
    resp.setHeader(HttpHeaders.Names.CONTENT_TYPE, "text/html")

    val body = <body style='margin-top: 3%; margin-bottom: 3%; margin-left: 2%; margin-right: 2%;'>
    <div style='font-family: Helvetica, sans-serif; font-weight: 300; line-height: 1.5em; width: 61.8%; color: hsl(0, 0%, 0%);'>
      <h2>Thor Imageserver</h2>
      <p>
      Thor is an image service that applies filters to existing images, e.g. artist/album covers.
      It works like any other image software, by combining layers with overlays and masks, and
      applying filters to layers.
      </p>

      <p>
      The layers parameter is the heart of Thor. You specify a list of layers, separated by 
      semicolon `;` characters. Each layer, has a source, which can either be an empty image
      (given by an underscore `_`), or a url, or another layer (given by `$i`, where i is the
      index. don't leave out the dollar sign). Each layer also has a filter, and a table of those
      filters is given below. The layer is specified by `&lt;source&gt;:&lt;filter&gt;`. If the
      source is not specified, the previous layer is referenced.
      </p>
    </div>
    <div style='font-family: Consolas, Liberation Mono, Menlo, Courier, monospace; margin-left: 4px; margin-top: 2em;'>
      <h3 style='margin-top: 1.5em; margin-bottom: 0.5em; color: hsl(0, 0%, 33%); line-height: 1.25em;'>Primitives</h3>
      <table style='font-size: 10pt; color: hsl(0, 0%, 33%); line-height: 1.25em;'>
        {
          for (filter <- parserFactory(0, 0).namedPrimitives) yield {
            <tr>
              {<td>
                {
                  val filterName = filter.toString
                  filterName.substring("Parser (".length, filterName.length-1)
                }
              </td>
              }
            </tr>
          }
        }
      </table>

      <h3 style='margin-top: 1.75em; margin-bottom: 0.5em; color: hsl(0, 0%, 33%); line-height: 1.25em;'>Filters</h3>
      <table style='font-size: 10pt; color: hsl(0, 0%, 33%); line-height: 1.25em;'>
        {
          for (filter <- parserFactory(0, 0).namedFilters) yield {
            <tr>
              {<td>
                {
                  val filterName = filter.toString
                  filterName.substring("Parser (".length, filterName.length-1)
                }
              </td>
              }
            </tr>
          }
        }
      </table>

      <h3 style='margin-top: 1.75em; margin-bottom: 0.5em; color: hsl(0, 0%, 33%); line-height: 1.25em;'>Layers</h3>
      <table style='font-size: 10pt; color: hsl(0, 0%, 33%); line-height: 1.25em;'>
        {
          for (filter <- parserFactory(0, 0).namedLayers) yield {
            <tr>
              {<td>
                {
                  val filterName = filter.toString
                  filterName.substring("Parser (".length, filterName.length-1)
                }
              </td>
              }
            </tr>
          }
        }
      </table>

      <h3 style='margin-top: 1.75em; margin-bottom: 0.5em; color: hsl(0, 0%, 33%); line-height: 1.25em;'>Examples</h3>
      <table style='font-size: 10pt; color: hsl(0, 0%, 33%); line-height: 1.25em;'>
      <tr><td>
        <p>
        // box blurs an image:<br />
        http://localhost:8080/?l=img_a.jpg%3Bboxblur(40px%2C40px)
        </p>
      </td></tr>
      
      <tr><td>
        <p>
        // Linearly interpolates between two images (img_a.jpg and img_b.jpg) using a linear gradient as a blending mask:<br />
        http://localhost:8080/?l=img_a.jpg%3Bimg_b.jpg%3B_%3Alinear(0deg%2C%20rgba(0%2C%200%2C%200%2C%200)%200%25%2C%20rgba(0%2C%200%2C%200%2C%201)%20100%25)%3B%240%3Amask(%241%2C%20%242)
        </p>
      </td></tr>
      
      <tr><td>
        <p>
        // draws a frame on the image, and draws some text on the image, fitted so that it doesn't exceed 75% of the width:<br />
        http://localhost:8080/l=img_a.jpg;$0:frame(16px,rgba(0,0,0,0.75));$1:text("Dooh dee dooh", bold italic 36px "Helvetica", rgb(0.75,0.25,0.25), [cartesian(9%,75%), cartesian(0px,4px)], left, top, fitted(75%,36px))
        </p>
      </td></tr>
      </table>
    </div>
    </body>
    resp.write(body.toString)
    Future.value(resp)
  }

  def handleImage(req: Request): Future[Response] = {
    req.params.get("l") match {
      case Some(layers) => {
        // Gather parameters
        val (width, height) = getDimensions(req.params.getInt("w"), req.params.getInt("h"))
        val compression = getCompression(req.params.getInt("c"))
        val format = getFormatter(req.params.get("f"))
        val parser = parserFactory(width.getOrElse(200), height.getOrElse(200))

        parser.parseAll(parser.layers, layers) match {
          case parser.Success(layers, _) => {
            val paths = extractPaths(layers).toArray

            requestImages(paths) map {
              potentialImages => {
                val fetchedImages = buildImageMap(paths, potentialImages.toArray)
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
              <p style="font-family: Helvetica; font-size: 10pt; padding: 12px; padding-left: 32px; margin-top: 2px; margin-bottom: 2px;background-color: hsl(0,0%,90%); color: hsl(0, 0%, 40%);">If you need the general reference, it is at <a href="/help">/help</a></p>
              <pre style="padding: 32px; margin-top: 2px; margin-bottom: 2px; background-color: hsl(0,0%,90%);"><code>${parseErrPretty}</code></pre>
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
        resp.write("""<p style="font-family: Helvetica; font-size: 10pt; padding: 12px; padding-left: 32px; margin-top: 2px; margin-bottom: 2px;background-color: hsl(0,0%,90%); color: hsl(0, 0%, 40%);">If you need the general reference, it is at <a href="/help">/help</a></p>""")
        Future.value(resp)
      }
    }
  }

  def apply(req: Request): Future[Response] = {
    // want to be able to match e.g. /halp!!11!!!
    if (req.path.startsWith("/help") || req.path.startsWith("/halp")) {
      handleHelp(req)
    } else {
      handleImage(req)
    }
  }
}
