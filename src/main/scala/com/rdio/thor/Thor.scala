package com.rdio.thor

import java.net.InetSocketAddress

import com.twitter.conversions.time._
import com.twitter.finagle.Service
import com.twitter.finagle.builder.{Server, ServerBuilder, ClientBuilder}
import com.twitter.finagle.http.{Http, RichHttp, Request, Response}
import com.twitter.conversions.storage._
import com.twitter.finagle.http.service.RoutingService

import com.typesafe.config.{Config, ConfigFactory}
import com.twitter.util.{Future}
import com.twitter.finagle.http.{Http, Status, RichHttp, Request, Response, Message}

import org.jboss.netty.handler.codec.http._
import collection.JavaConversions._

class MultiOriginImageService(clients: Map[String, Service[Request, Response]]) extends Service[Request, Response] {
  def apply(req: Request): Future[Response] = {
    req.userAgent = "Thor-Imageserver"
    req.accept = "image/*"

    req.host match {
      case None => Future.value(Response(HttpVersion.HTTP_1_1, HttpResponseStatus.BAD_REQUEST))
      case Some(addr) => {
        clients.get(addr) match {
          case None => Future.value(Response(HttpVersion.HTTP_1_1, HttpResponseStatus.NOT_FOUND))
          case Some(client) => client(req)
        }
      }
    }   
  }
}

/** Main entry-point for the server. Builds the request-response flow and starts the server. */
object Thor extends App {
  val conf: Config = ConfigFactory.load()

  val maxResponseSize = if (conf.hasPath("MAX_RESPONSE_SIZE_IN_MB")) {
    conf.getInt("MAX_RESPONSE_SIZE_IN_MB")
  } else {
    2
  }

  val hosts = conf.getStringList("IMAGESERVER_ALLOWED_HOSTS")
  val client =
    if (hosts.length == 1) {
      ClientBuilder()
        .codec(RichHttp[Request](Http().maxResponseSize(maxResponseSize.megabytes)))
        .hosts(hosts.head)
        .hostConnectionLimit(conf.getInt("HOST_CONNECTION_LIMIT"))
        .name("thor-client")
        .build()
    } else {
      val clients = hosts.map { (url: String) =>
        (url, ClientBuilder()
          .codec(RichHttp[Request](Http().maxResponseSize(maxResponseSize.megabytes)))
          .hosts(url)
          .hostConnectionLimit(conf.getInt("HOST_CONNECTION_LIMIT"))
          .name("thor-client")
          .build())
      }.toMap

      new MultiOriginImageService(clients)
    }
    

  val server = ServerBuilder()
    .codec(RichHttp[Request](Http()))
    .bindTo(new InetSocketAddress(conf.getInt("IMAGESERVER_PORT")))
    .name("thor-server")
    .build(new ImageService(conf, client))
}
