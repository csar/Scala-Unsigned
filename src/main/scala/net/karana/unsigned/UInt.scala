package net.karana.unsigned

object UInt {
  implicit def toUInt(byte: Int) = UInt(byte)
}

case class UInt(val int: Int) extends UIntIsIntegral {
  val self                 = mkNumericOps(this)
  def >>(n: Int): UInt     = (int >>> n).toInt
  def >>>(n: Int)          = >> _
  def <<(n: Int): UInt     = ((int << n) & 0xffff).toInt
  def +(other: UInt): UInt = self + other
  def -(other: UInt): UInt = self - other
  def unary_- : UInt       = negate(this)
  def *(other: UInt): UInt = self * other
  def /(other: UInt): UInt = self / other
  def toShort: Short       = int.toShort
  def toByte: Byte       = int.toByte
  def toChar: Char         = int.toChar
  def toInt: Int           = int
  def toLong: Long         = self.toLong()
  override def toString()  = toLong.toString()

}
