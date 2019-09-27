/*
 * Scala Radius Client
 * Created on 27/07/2019
 *
 * @author Bilal Pierre ABDELKADER
 * @version: 1.0.0
 */
package com.ngenia.radiusclient

import com.ngenia.radiusclient.client.RadiusClient
import com.ngenia.radiusclient.packet._
import org.scalatest.FunSuite
import org.scalatest.Matchers._

import scala.collection.immutable.Map

class TestSuite1 extends FunSuite {

  val radiusClient = RadiusClient("140.27.30.40",
    "mySharedSecret",
    3000) // TimeOut in ms

  val accessRequestAttributes = Map(
    "NAS-Identifier" -> "OPERATOR1",
    "NAS-IP-Address" -> "10.11.12.13",
    "Calling-Station-Id" -> "0123456789",
    "NAS-Port" -> "1812",
    "Framed-Protocol" -> "Gandalf-SLML"
  )

  val accountingRequestAttributes = Map(
    "NAS-Identifier" -> "OPERATOR1",
    "NAS-IP-Address" -> "10.11.12.13",
    "Calling-Station-Id" -> "0123456789",
    "Framed-IP-Address" -> "21.52.73.4"
  )

  test("Assert: Create Radius Client") {
    radiusClient shouldBe a [RadiusClient]
  }

  test("Assert: Send [ACCESS_REQUEST] -> [ACCESS_ACCEPT]") {
    assert(
      radiusClient
        .authenticate(AccessRequest.AUTH_CHAP, "login0001", "pass0001", accessRequestAttributes)
        .get
        .code eq RadiusPacket.ACCESS_ACCEPT)
  }

  test("Assert: Send [ACCOUNTING_START] -> [ACCOUNTING_RESPONSE]") {
    assert(
      radiusClient
        .account(AccountingRequest.ACCOUNT_START, "login0001", accountingRequestAttributes)
        .get
        .code eq RadiusPacket.ACCOUNTING_RESPONSE)
  }

  test("Assert: Send [ACCOUNTING_INTERIM] -> [ACCOUNTING_RESPONSE]") {
    assert(
      radiusClient
        .account(AccountingRequest.ACCOUNT_INTERIM, "login0001", accountingRequestAttributes)
        .get
        .code eq RadiusPacket.ACCOUNTING_RESPONSE)
  }

  test("Assert: Send [ACCOUNTING_STOP] -> [ACCOUNTING_RESPONSE]") {
    assert(
      radiusClient
        .account(AccountingRequest.ACCOUNT_STOP, "login0001", accountingRequestAttributes)
        .get
        .code eq RadiusPacket.ACCOUNTING_RESPONSE)
  }

}