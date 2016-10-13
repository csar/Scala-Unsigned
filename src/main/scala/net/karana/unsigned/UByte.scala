package net.karana.unsigned

object UByte {
  implicit def toUByte(byte: Byte): UByte = UByte(byte)
  implicit def toUByte(byte: Int): UByte  = UByte(byte.toByte)
//  implicit object UByteIsIntegral extends UByteIsIntegral

}

case class UByte(val byte: Byte) extends UByteIsIntegral {
  val self                   = mkNumericOps(this)
  def >>(n: Int): UByte      = (byte >>> n).toByte
  def >>>(n: Int)            = >> _
  def <<(n: Int): UByte      = ((byte << n) & 0xff).toByte
  def +(other: UByte): UByte = self + other
  def -(other: UByte): UByte = self - other
  def unary_- : UByte        = negate(this)
  def *(other: UByte): UByte = self * other
  def /(other: UByte): UByte = self / other
  def toInt: Int             = self.toInt()
  def toChar: Char           = toInt.toChar
  def toShort: Short         = toInt.toShort
  def toLong: Long           = toInt
  override def toString()    = toInt.toString()
}
