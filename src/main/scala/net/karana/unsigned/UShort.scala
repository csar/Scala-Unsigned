package net.karana.unsigned

object UShort {
  implicit def toUShort(byte:Short) = UShort(byte)
}



case class UShort(val short:Short) extends UShortIsIntegral {
  val self = mkNumericOps(this)
  def >>(n:Int):UShort = (short>>>n).toShort
  def >>>(n:Int) = >> _
  def <<(n:Int):UShort = ((short<<n)&0xffff).toShort
  def + (other:UShort):UShort = self + other
  def - (other:UShort):UShort = self - other
  def unary_- :UShort = negate(this)
  def * (other:UShort):UShort = self * other
//  def / (other:UShort):UShort = self /other
  def toInt:Int = if (short<0) 65536+short else short
  def toChar:Char = toInt.toChar
  def toShort:Short = toInt.toShort
  def toLong:Long = toInt
  override def toString() = toInt.toString()

}