/*
 * Scala Radius Client
 * Created on 27/07/2019
 *
 * @author Bilal Pierre ABDELKADER
 * @version: 1.0.0
 */
package com.ngenia.radiusclient.packet

object AccessRequest extends Enumeration {

  val AUTH_CHAP = Value(1)
  val AUTH_PAP = Value(2)

  def apply(authenticationProtocol: AccessRequest.Value,
            userName: String,
            password: String,
            sharedSecret: String): AccessRequest =
    new AccessRequest(authenticationProtocol, userName, password, sharedSecret)
      .getPacketIdentifier()
      .setUsername(userName)
      .asInstanceOf[AccessRequest]
      .encodePassword(authenticationProtocol, password, sharedSecret)
      .asInstanceOf[AccessRequest]
}

case class AccessRequest(authenticationProtocol: AccessRequest.Value,
                         userName: String,
                         password: String,
                         sharedSecret: String
                        ) extends RadiusPacket(RadiusPacket.ACCESS_REQUEST) {

  def encodePassword(authenticationProtocol: AccessRequest.Value, password: String, sharedSecret: String): RadiusPacket = {
    authenticationProtocol match {
      case AccessRequest.AUTH_CHAP => encodeChapPassword(password)
      case AccessRequest.AUTH_PAP => encodePapPassword(password, sharedSecret)
    }
  }
}
