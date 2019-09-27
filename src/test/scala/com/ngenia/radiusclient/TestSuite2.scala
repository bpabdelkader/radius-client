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

class TestSuite2 extends FunSuite {

  val radiusClient = RadiusClient("140.27.30.40",
    "mySharedSecret",
    3000) // TimeOut in ms

  val radiusAttributes = Map(
    "3GPP-SGSN-Address" -> "1.2.3.4",
    "3GPP-IMSI-MCC-MNC" -> "LAN"
  )

  test("Creating Radius Client") {
    radiusClient shouldBe a[RadiusClient]
  }

  val accessRequest = AccessRequest(AccessRequest.AUTH_CHAP, "login0001", "pass0001", "mySharedSecret")
  accessRequest.addAttributes(radiusAttributes)

  accessRequest.addAttribute("NAS-Identifier", "OPERATOR1")
  accessRequest.addAttribute("NAS-IP-Address", "10.11.12.13")
  accessRequest.addAttribute("Calling-Station-Id", "0123456789")
  accessRequest.addAttribute("NAS-Port", "1812")

  test("Assert: Send [ACCESS_REQUEST] -> [ACCESS_ACCEPT]") {
    assert(
      radiusClient
        .authenticate(accessRequest)
        .get
        .code eq RadiusPacket.ACCESS_ACCEPT
    )
  }

  val accountingRequest = AccountingRequest(AccountingRequest.ACCOUNT_START, "login0001")
  accountingRequest.addAttributes(radiusAttributes)

  accountingRequest.addAttribute("NAS-Identifier", "OPERATOR1")
  accountingRequest.addAttribute("NAS-IP-Address", "10.11.12.13")
  accountingRequest.addAttribute("Calling-Station-Id", "0123456789")
  accountingRequest.addAttribute("NAS-Port", "1813")
  accountingRequest.addAttribute("Framed-IP-Address", "1.2.3.4")

  test("Assert: Send [ACCOUNTING_START] -> [ACCOUNTING_RESPONSE]") {
    assert(
      radiusClient
        .account(accountingRequest)
        .get
        .code eq RadiusPacket.ACCOUNTING_RESPONSE)
  }
}
