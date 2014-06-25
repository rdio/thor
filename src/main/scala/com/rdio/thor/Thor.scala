package com.rdio.thor

import java.net.InetSocketAddress

import com.twitter.conversions.time._
import com.twitter.finagle.Service
import com.twitter.finagle.builder.{Server, ServerBuilder, ClientBuilder}
import com.twitter.finagle.http.{Http, RichHttp, Request, Response}

import com.typesafe.config.{Config, ConfigFactory}

import org.jboss.netty.handler.codec.http._

/** Main entry-point for the server. Builds the request-response flow and starts the server. */
object Thor extends App {
  val conf: Config = ConfigFactory.load()

  val client = ClientBuilder()
    .codec(RichHttp[Request](Http()))
    .hosts(new InetSocketAddress(
      conf.getString("IMAGESERVER_MEDIA_HOST"),
      conf.getInt("IMAGESERVER_MEDIA_PORT")))
    .name("thor-client")
    .build()

  val server = ServerBuilder()
    .codec(RichHttp[Request](Http()))
    .bindTo(new InetSocketAddress(conf.getInt("IMAGESERVER_PORT")))
    .name("thor-server")
    .build(new ImageService(conf, client))
}
