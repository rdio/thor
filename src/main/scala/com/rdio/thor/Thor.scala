package com.rdio.thor

import java.net.InetSocketAddress

import com.twitter.conversions.time._
import com.twitter.finagle.Service
import com.twitter.finagle.builder.{Server, ServerBuilder, ClientBuilder}
import com.twitter.finagle.http.{Http, RichHttp, Request, Response}
import com.twitter.conversions.storage._

import com.typesafe.config.{Config, ConfigFactory}

import org.jboss.netty.handler.codec.http._
import collection.JavaConversions._

/** Main entry-point for the server. Builds the request-response flow and starts the server. */
object Thor extends App {
  val conf: Config = ConfigFactory.load()

  val clients = conf.getStringList("IMAGESERVER_ALLOWED_HOSTS").map { (url: String) =>
    (url, ClientBuilder()
      .codec(RichHttp[Request](Http().maxResponseSize(conf.getInt("MAX_RESPONSE_SIZE_IN_MB").megabytes)))
      .hosts(url)
      .hostConnectionLimit(conf.getInt("HOST_CONNECTION_LIMIT"))
      .name("thor-client")
      .build())
  }.toMap

  val server = ServerBuilder()
    .codec(RichHttp[Request](Http()))
    .bindTo(new InetSocketAddress(conf.getInt("IMAGESERVER_PORT")))
    .name("thor-server")
    .build(new ImageService(conf, clients))
}
