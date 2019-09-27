/*
 * Scala Radius Client
 * Created on 27/07/2019
 *
 * @author Bilal Pierre ABDELKADER
 * @version: 1.0.0
 */
package com.ngenia.radiusclient.utils

import java.security.MessageDigest

object MD5 {

  def getRandom(size: Int): Array[Byte] = Array.fill(size)((scala.util.Random.nextInt(256) - 128).toByte)

  def getMD5Digest(): MessageDigest = MessageDigest.getInstance("MD5")

  def getMD5Digest(sharedSecret: String, message: Option[Array[Byte]] = None): Array[Byte] = {
    val digest = getMD5Digest
    if (message.isDefined)
      digest.update(message.get)
    digest.digest(Util.string2Bytes(sharedSecret))
  }

  /*
   * As per as per RFC 2865/5.3.CHAP-Password & 5.40.CHAP-Challenge
   * (https://datatracker.ietf.org/doc/rfc2865/?include_text=1)
   */
  def getMD5CHAPPassword(password: String, chapChallenge: Array[Byte]): Array[Byte] = {
    val digest = getMD5Digest
    val chapIdent = getRandom(1)
    digest.update(chapIdent ++ Util.string2Bytes(password))
    chapIdent ++ digest.digest(chapChallenge)
  }

  /*
   * As per as per RFC 2865/5.2.User-Password
   */
  def getMD5PAPPassword(encryptedPassword: Array[Byte], sharedSecret: String): Array[Byte] = {
    var tempBuffer: Array[Byte] = Array.ofDim(16)
    var tempDigest: Array[Byte] = Array()
    val digest = MessageDigest.getInstance("MD5")
    for (i <- 0 until encryptedPassword.length by 16) {
      digest.update(Util.string2Bytes(sharedSecret))
      digest.digest(if (i == 0) getMD5Digest(sharedSecret) else tempBuffer)
      tempDigest = digest.digest()
      tempBuffer = encryptedPassword.slice(i, i + 16)
      // XOR
      for (j <- 0 until 16) encryptedPassword(i + j) = (encryptedPassword(i + j) ^ tempDigest(j)).toByte
    }
    encryptedPassword
  }

}