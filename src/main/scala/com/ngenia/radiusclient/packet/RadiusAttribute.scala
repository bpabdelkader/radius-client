/*
 * Scala Radius Client
 * Created on 27/07/2019
 *
 * @author Bilal Pierre ABDELKADER
 * @version: 1.0.0
 */
package com.ngenia.radiusclient.packet

import com.ngenia.radiusclient.enum.RadiusClassTag
import com.ngenia.radiusclient.utils.Util
import com.typesafe.scalalogging.LazyLogging

import scala.util.{Failure, Success, Try}

case class AttributeHeader(vendorId: Int,
                           name: String,
                           code: Int,
                           classTag: String,
                           var enumValues: Map[String, String] = Map.empty
                          ) extends LazyLogging {

  def getValue(value: String): Option[String] = Try(
    if (this.hasEnumValues()) enumValues.find({ case (a, _) => a == value }).get._2 else value
  ) match {
    case Success(x) => Some(x)
    case Failure(e) => {
      logger.error(s"[Packet Construction]: Unknown Enum Attribute: ${value}")
      None
    }
  }

  def getValue(value: Array[Byte]): String = if (this.hasEnumValues()) enumValues.find({ case (_, b) => b == Util.int2String(value) }).get._1 else Util.toString(classTag, value)

  private def hasEnumValues(): Boolean = !enumValues.isEmpty
}

trait Attribute {
  val attributeHeader: AttributeHeader

  def serialize: Array[Byte]
}

case class RadiusAttribute(attributeHeader: AttributeHeader,
                           length: Byte,
                           value: Array[Byte]
                          ) extends Attribute {

  override def serialize: Array[Byte] = Array[Byte](attributeHeader.code.toByte, length) ++ value

  override def toString(): String = attributeHeader.name + ": " + attributeHeader.getValue(value)
}

case class VendorAttribute(vendorId: Int,
                           attributeHeader: AttributeHeader = AttributeHeader(0, "Vendor-Specific", 26, RadiusClassTag.BYTES),
                           var child: RadiusAttribute = null
                          ) extends Attribute {

  def addRadiusAttribute(radiusAttribute: RadiusAttribute): this.type = {
    child = radiusAttribute
    this
  }

  override def serialize: Array[Byte] = Array[Byte](26.toByte, (child.length + 6).toByte) ++ Util.int2Bytes(vendorId) ++ child.serialize

  override def toString(): String = attributeHeader.name + " (" + vendorId + "):\n\t\t  " + child.toString()
}