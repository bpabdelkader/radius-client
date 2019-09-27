/*
 * Scala Radius Client
 * Created on 27/07/2019
 *
 * @author Bilal Pierre ABDELKADER
 * @version: 1.0.0
 */
package com.ngenia.radiusclient.packet

import com.ngenia.radiusclient.dictionary.RadiusDictionary
import com.ngenia.radiusclient.enum.{RadiusClassTag, RadiusParameter}
import com.ngenia.radiusclient.utils.{MD5, Util}
import com.typesafe.scalalogging.LazyLogging

import scala.collection.immutable.Map
import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success, Try}

object RadiusPacket extends Enumeration {
  var identifier: Byte = 0

  val ACCESS_REQUEST = Value(1)
  val ACCESS_ACCEPT = Value(2)
  val ACCESS_REJECT = Value(3)
  val ACCOUNTING_REQUEST = Value(4)
  val ACCOUNTING_RESPONSE = Value(5)
  val ACCOUNTING_STATUS = Value(6)
  val PASSWORD_REQUEST = Value(7)
  val PASSWORD_ACCEPT = Value(8)
  val PASSWORD_REJECT = Value(9)
  val ACCOUNTING_MESSAGE = Value(10)
  val ACCESS_CHALLENGE = Value(11)
  val STATUS_SERVER = Value(12)
  val STATUS_CLIENT = Value(13)
  val DISCONNECT_REQUEST = Value(40)
  val DISCONNECT_ACK = Value(41)
  val DISCONNECT_NAK = Value(42)
  val COA_REQUEST = Value(43)
  val COA_ACK = Value(44)
  val COA_NAK = Value(45)
  val STATUS_REQUEST = Value(46)
  val STATUS_ACCEPT = Value(47)
  val STATUS_REJECT = Value(48)
  val RESERVED = Value(255)
}

class RadiusPacket(
                    val code: RadiusPacket.Value,
                    var identifier: Byte = 0,
                    var length: Short = RadiusParameter.RADIUS_HEADER_LENGTH,
                    var authenticator: Array[Byte] = Array.ofDim[Byte](16),
                    var attributes: ArrayBuffer[Attribute] = ArrayBuffer[Attribute]()
                  ) extends LazyLogging {

  def getPacketIdentifier(): RadiusPacket = Option(RadiusPacket.identifier + 1 % 255)
    .map {
      identifier =>
        RadiusPacket.identifier = identifier
        this.identifier = RadiusPacket.identifier
        this
    }.get

  def setUsername(username: String): RadiusPacket = addAttribute("User-Name", username)

  protected def encodeChapPassword(password: String): RadiusPacket = Option(MD5.getRandom(16))
    .map {
      chapChallenge => {
        addAttribute("CHAP-Challenge", chapChallenge)
        addAttribute("CHAP-Password", MD5.getMD5CHAPPassword(password, chapChallenge))
      }
    }.get

  protected def encodePapPassword(password: String, sharedSecret: String): RadiusPacket = Option(Util.string2Bytes(password))
    .map {
      passwordBytes =>
        addAttribute("User-Password",
          MD5.getMD5PAPPassword(
            passwordBytes.take(math.ceil((passwordBytes.length % 16) * 16).toInt),
            sharedSecret)
        )
    }.get

  /*
  * Packet Authenticator
  *
  * RFC 2865 -> 3. Packet Format
  * In Access-Request Packets, the Authenticator value is a 16
  * octet random number, called the Request Authenticator.
  *
  * RFC 2866 -> 3. Packet Format
  * The Request Authenticator field in Accounting-Request packets contains a one-
  * way MD5 hash calculated over a stream of octets consisting of the
  * Code + Identifier + Length + 16 zero octets + request attributes +
  * shared secret (where + indicates concatenation).  The 16 octet MD5
  * hash value is stored in the Authenticator field of the
  * Accounting-Request packet.
  */
  def addAuthenticator(sharedSecret: String): Unit = this match {
    case x: AccessRequest => authenticator = MD5.getMD5Digest(sharedSecret)
    case x: AccountingRequest => authenticator = MD5.getMD5Digest(sharedSecret, Option(serializePacket()))
  }

  /*
    Add attribute(s) to radius packet
   */
  def addAttributes(radiusAttributes: Map[String, String]): Unit = for ((k, v) <- radiusAttributes) addAttribute(k, v)

  def addAttribute(typeName: String, value: Any): RadiusPacket = RadiusDictionary.getAttributeHeader(typeName)
    .map {
      attributeHeader => {
        value match {
          case x: String => addAttribute(attributeHeader, Util.toBytes(attributeHeader.classTag, attributeHeader.getValue(x)).orNull).orNull
          case x: Array[Byte] => addAttribute(attributeHeader, x).orNull
        }
      }
    }.orNull

  private def addAttribute(attributeHeader: AttributeHeader, value: Array[Byte]): Option[RadiusPacket] = {
    Try {
      for (radiusAttribute <- Option(RadiusAttribute(attributeHeader, value.length + 2, value)))
        yield {
          attributeHeader.vendorId match {
            case 0 => attributes += radiusAttribute
            case _ => {
              attributes += VendorAttribute.apply(attributeHeader.vendorId).addRadiusAttribute(radiusAttribute)
              length = (length + 6 & 255).toShort
            }
          }
          length = (length + radiusAttribute.length & 255).toShort
        }
    }
  } match {
    case Success(_) => Some(this)
    case Failure(e) => {
      logger.error(s"[Packet Serialisation]: Error while creating Radius Attribute: ${attributeHeader.name}")
      None
    }
  }

  def addAttribute(radiusAttributeCode: Byte, value: Array[Byte]): Option[RadiusPacket] = radiusAttributeCode match {
    case 26 => addAttribute(RadiusDictionary.getAttributeHeader(Util.bytes2Int(value.take(4)), value(4)).get, value.drop(6))
    case _ => addAttribute(RadiusDictionary.getAttributeHeader(radiusAttributeCode).orNull, value)
  }

  def serializePacket(): Array[Byte] = Array[Byte](code.id, identifier) ++
    Util.short2Bytes(length) ++
    authenticator ++
    attributes
      .toArray
      .map({ attribute => attribute.serialize })
      .flatMap(_.toList)

  def deserializeRadiusPacket(receivedPacket: Array[Byte], sharedSecret: String): Option[RadiusPacket] = {
    val code = RadiusPacket(receivedPacket(0))
    val radiusPacket = new RadiusPacket(code)
    radiusPacket.identifier = receivedPacket(1)
    radiusPacket.authenticator = receivedPacket.slice(4, RadiusParameter.RADIUS_HEADER_LENGTH)
    var position = RadiusParameter.RADIUS_HEADER_LENGTH
    while (position < ((receivedPacket(2) & 0x0ff) | receivedPacket(3) & 0x0ff)) {
      radiusPacket.addAttribute(receivedPacket(position), receivedPacket.slice(position + 2, position + receivedPacket(position + 1)))
      position += receivedPacket(position + 1)
    }

    // Check the Response Integrity
    if (radiusPacket.authenticator.sameElements(receivedPacket.slice(4, RadiusParameter.RADIUS_HEADER_LENGTH)))
      logger.info(s"[Response Validation]: ${code} Successfully Authenticated")
    else
      logger.info(s"[Response Validation]: ${code}'s Authenticator is Not Valid")
    Some(radiusPacket)
  }

  override def toString(): String = (new StringBuffer)
    .append("Type: " + code)
    .append("\nIdentifier: " + identifier)
    .append("\nAuthenticator: " + Util.toString(RadiusClassTag.BYTES, authenticator))
    .append("\nAttributes: ") +
    (for (attribute <- attributes) yield "\n\t\t" + attribute.toString).mkString(" ")

  implicit def int2Byte(int: Int): Byte = Util.int2Byte(int)

  implicit def int2Short(int: Int): Short = Util.int2Short(int)
}