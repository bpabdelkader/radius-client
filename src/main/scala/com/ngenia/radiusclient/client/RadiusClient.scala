/*
 * Scala Radius Client
 * Created on 27/07/2019
 *
 * @author Bilal Pierre ABDELKADER
 * @version: 1.0.0
 */
package com.ngenia.radiusclient.client

import com.ngenia.radiusclient.packet.{AccessRequest, AccountingRequest, RadiusPacket}
import com.typesafe.scalalogging.LazyLogging

import scala.collection.immutable.Map

case class RadiusClient(
                         host: String,
                         sharedSecret: String,
                         timeOut: Int
                       ) extends LazyLogging {

  def authenticate(authProtocol: AccessRequest.Value = AccessRequest.AUTH_CHAP, userName: String, password: String, radiusAttributes: Map[String, String]): Option[RadiusPacket] = Option(AccessRequest(authProtocol, userName, password, sharedSecret))
    .map {
      accessRequest => {
        accessRequest.addAttributes(radiusAttributes)
        authenticate(accessRequest)
      }
    }.orNull

  def authenticate(accessRequest: AccessRequest): Option[RadiusPacket] = {
    accessRequest.addAuthenticator(sharedSecret)
    send(accessRequest, 1812)
  }

  def account(radiusAccountingType: AccountingRequest.Value, userName: String, radiusAttributes: Map[String, String]): Option[RadiusPacket] = Option(AccountingRequest(radiusAccountingType, userName))
    .map {
      accountingRequest => {
        accountingRequest.addAttributes(radiusAttributes)
        account(accountingRequest)
      }.orNull
    }

  def account(accountingRequest: AccountingRequest): Option[RadiusPacket] = {
    accountingRequest.addAuthenticator(sharedSecret)
    send(accountingRequest, 1813)
  }

  private def send(radiusPacket: RadiusPacket, port: Int): Option[RadiusPacket] = {
    print("Packet Sent", radiusPacket)
    RadiusEndpoint(host, port, timeOut, sharedSecret).send(radiusPacket).map {
      response =>
        if (response.isInstanceOf[RadiusPacket])
          print("Packet Received", response)
        response
    }
  }

  private def print(message: String, radiusPacket: RadiusPacket) = logger.info(
    "\n---------------- ----------------------- ----------------\n" +
      "\t\t\t\t\t\t[" + message +
      "]\n---------------- ----------------------- ----------------\n" +
      radiusPacket +
      "\n---------------- ----------------------- ----------------\n"
  )
}