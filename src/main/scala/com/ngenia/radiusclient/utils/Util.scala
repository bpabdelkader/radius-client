/*
 * Scala Radius Client
 * Created on 27/07/2019
 *
 * @author Bilal Pierre ABDELKADER
 * @version: 1.0.0
 */
package com.ngenia.radiusclient.utils

import java.net.InetAddress
import java.nio.ByteBuffer

import com.ngenia.radiusclient.enum.RadiusClassTag
import com.typesafe.scalalogging.LazyLogging

import scala.util.{Failure, Success, Try}

object Util extends LazyLogging {

  def toBytes(classTag: String, value: Option[String]): Option[Array[Byte]] = {
    if (value.isDefined)
      Try {
        classTag match {
          case RadiusClassTag.INT =>  int2Bytes(value.get)
          case RadiusClassTag.IPv4 => if (isIpAddress(value.get)) string2InetAddress(value.get) else throw new Throwable
          case _ => string2Bytes(value.get)
        }
      } match {
        case Success(value) => Some(value)
        case Failure(e) => {
          logger.error(s"[Packet Serialisation]: Attribute Value ${value.get} is not an $classTag")
          None
        }
      }
    else None
  }

  def toString(classTag: String, value: Array[Byte]): String = {
    Try {
      classTag match {
        case RadiusClassTag.INT => int2String(value)
        case RadiusClassTag.IPv4 => inetAddress2String(value)
        case RadiusClassTag.BYTES => bytes2Hexa(value)
        case _ => bytes2String(value)
      }
    } match {
      case Success(value) => value
      case Failure(e) => {
        logger.error(s"[Packet DeSerialisation]: Attribute Value $value. is not an $classTag")
        null
      }
    }
  }

  def short2Bytes(short: Short): Array[Byte] = Array(((short & 0xFF00) >> 8).toByte, (short & 0x00FF).toByte)

  def int2Bytes(int: Int): Array[Byte] = java.nio.ByteBuffer.allocate(4).putInt(int).array()

  def int2Byte(int: Int): Byte = int.toByte

  def int2Short(int: Int): Short = int.toShort

  def string2Bytes(string: String): Array[Byte] = string.getBytes("UTF-8")

  def bytes2Int(bytes: Array[Byte]): Int = ByteBuffer.wrap(bytes).getInt()

  def bytes2String(bytes: Array[Byte]): String = new String(bytes, "UTF-8")

  def int2String(bytes: Array[Byte]): String = java.nio.ByteBuffer.wrap(bytes).getInt.toString

  private def int2Bytes(string: String): Array[Byte] = java.nio.ByteBuffer.allocate(4).putInt(string.toInt).array()

  private def string2InetAddress(string: String): Array[Byte] = InetAddress.getByName(string).getAddress

  private def isIpAddress(string: String) = string.matches("^[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}")

  private def bytes2Hexa(bytes: Array[Byte]): String = "0x" + bytes.map("%02x".format(_)).mkString

  private def inetAddress2String(bytes: Array[Byte]): String = InetAddress.getByAddress(bytes).getHostAddress
}