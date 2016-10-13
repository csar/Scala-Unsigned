package net.karana.unsigned

import scala.math.BigDecimal

object ULong {
  implicit def toULong(byte: Long) = ULong(byte)
}

case class ULong(val long: Long) extends ULongIsIntegral {
  val self                   = mkNumericOps(this)
  def >>(n: Long): ULong     = (long >>> n).toLong
  def >>>(n: Long)           = >> _
  def <<(n: Long): ULong     = (long << n).toLong
  def +(other: ULong): ULong = self + other
  def -(other: ULong): ULong = self - other
  def unary_- : ULong        = negate(this)
  def *(other: ULong): ULong = self * other
  def /(other: ULong): ULong = self / other
  def toShort: Short         = long.toShort
  def toChar: Char           = long.toChar
  def toInt: Int             = long.toInt
  def toLong: Long           = long
  def toBigInt               = (BigInt(long >>> 1) << 1) + (long & 1)
  def toBigDecimal           = BigDecimal(toBigInt)
  override def toString() =
    if (long < 0) toBigInt.toString() else long.toString()

}
