/*
 * Scala Radius Client
 * Created on 27/07/2019
 *
 * @author Bilal Pierre ABDELKADER
 * @version: 1.0.0
 */
package com.ngenia.radiusclient.dictionary

import com.ngenia.radiusclient.packet.AttributeHeader
import com.typesafe.scalalogging.LazyLogging

import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success, Try}

object RadiusDictionary extends LazyLogging {

  private def loadDictionary(): Option[ArrayBuffer[AttributeHeader]] = {
    var attributes: ArrayBuffer[AttributeHeader] = ArrayBuffer[AttributeHeader]()

    Try(
      io.Source
        .fromFile(getClass.getResource("/com/ngenia/radiusclient/dictionary.dic").getPath)
        .mkString
        .split("\\r\\n")
        .filter(!_.isEmpty)
        .filter(!_.matches("^(#.*)"))
        .map(_.trim.split("\\s+"))
        .map {
          case Array(v1, v2, v3) => {
            Try(v2.toInt)
            match {
              case Success(v2) => attributes += AttributeHeader(0, v1, v2, v3)
              case Failure(e) => attributes.find(x => x.name == v1).get.enumValues += (v2 -> v3)
            }
          }
          case Array(v1, v2, v3, v4) => attributes += AttributeHeader(v1.toInt, v2, v3.toInt, v4)
        }
    ) match {
      case Success(_) => Some(attributes)
      case Failure(e) => {
        logger.error(s"[Dictionary]: Error while reading dictionary => ${e.getLocalizedMessage}")
        System.exit(0)
        None
      }
    }
  }

  private val attributes: ArrayBuffer[AttributeHeader] = loadDictionary().get

  def getAttributeHeader(x: Any): Option[AttributeHeader] =
    (x match {
      case name: String => attributes.find(x => x.name == name)
      case code: Byte => attributes.find(x => x.code.toByte == code)
      case (vendorId: Int, code: Byte) => attributes.find(x => (x.vendorId == vendorId) & (x.code.toByte == code))
    }) match {
      case None => {
        logger.error(s"[Packet Construction]: Unknown Attribute: ${x}")
        None
      }
      case x => logger.info(s"[Packet Construction]: Processing Attribute [${x.get.name}]")
        x
    }
}