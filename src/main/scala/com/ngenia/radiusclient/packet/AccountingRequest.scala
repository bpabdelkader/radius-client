/*
 * Scala Radius Client
 * Created on 27/07/2019
 *
 * @author Bilal Pierre ABDELKADER
 * @version: 1.0.0
 */
package com.ngenia.radiusclient.packet

object AccountingRequest extends Enumeration {

  val ACCOUNT_START         = Value("Start")
  val ACCOUNT_STOP          = Value("Stop")
  val ACCOUNT_INTERIM       = Value("Interim-Update")
  val ACCOUNT_ON            = Value("Accounting-On")
  val ACCOUNT_OFF           = Value("Accounting-Off")

  def apply(radiusAccounting: AccountingRequest.Value, userName: String): AccountingRequest =
    new AccountingRequest(radiusAccounting, userName)
      .getPacketIdentifier()
      .setUsername(userName)
      .addAttribute("Acct-Status-Type", radiusAccounting.toString)
      .asInstanceOf[AccountingRequest]
}

case class AccountingRequest(radiusAccounting: AccountingRequest.Value, userName: String) extends RadiusPacket(RadiusPacket.ACCOUNTING_REQUEST)