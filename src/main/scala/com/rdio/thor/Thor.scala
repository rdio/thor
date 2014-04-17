package com.rdio.thor

import java.net.InetSocketAddress

import com.twitter.conversions.time._
import com.twitter.finagle.builder.{Server, ServerBuilder}
import com.twitter.finagle.http.{Http, RichHttp, Request, Response}

import com.typesafe.config.{Config, ConfigFactory}

import org.jboss.netty.handler.codec.http._

/** Main entry-point for the server. Builds the request-response flow and starts the server. */
object Thor extends App {
  val conf: Config = ConfigFactory.load()

  val port: Int = conf.getInt("IMAGESERVER_PORT")

  val server: Server = ServerBuilder()
    .codec(RichHttp[Request](Http()))
    .bindTo(new InetSocketAddress(port))
    .name("thor-server")
    .build(new ImageService(conf))
}
