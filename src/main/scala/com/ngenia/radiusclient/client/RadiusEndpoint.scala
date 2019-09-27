/*
 * Scala Radius Client
 * Created on 27/07/2019
 *
 * @author Bilal Pierre ABDELKADER
 * @version: 1.0.0
 */
package com.ngenia.radiusclient.client

import java.net.{DatagramPacket, DatagramSocket, InetAddress}

import com.ngenia.radiusclient.enum.RadiusParameter
import com.ngenia.radiusclient.packet.RadiusPacket
import com.typesafe.scalalogging.LazyLogging

import scala.util.{Failure, Success, Try}

case class RadiusEndpoint(
                           val hostName: String,
                           val port: Int,
                           val timeOut: Int = 3,
                           val sharedSecret: String
                         ) extends LazyLogging {

  def send(radiusPacket: RadiusPacket): Option[RadiusPacket] =
    Try {
      var receivedPacket = new DatagramPacket(Array.ofDim[Byte](RadiusParameter.RADIUS_PACKET_MAX_LENGTH), (RadiusParameter.RADIUS_PACKET_MAX_LENGTH))
      val socket: DatagramSocket = (new DatagramSocket())
      val payload = radiusPacket.serializePacket()
      val sentPacket = new DatagramPacket(payload, payload.length, InetAddress.getByName(hostName), port)
      socket.setSoTimeout(timeOut)
      socket.send(sentPacket)
      socket.receive(receivedPacket)
      socket.close()
      radiusPacket.deserializeRadiusPacket(receivedPacket.getData, sharedSecret).orNull
    } match {
      case Success(x) => Some(x)
      case Failure(e) => {
        logger.error(s"[Packet Send/Receive]: ${e.getMessage}")
        print(e.getMessage)
        None
      }
    }
}