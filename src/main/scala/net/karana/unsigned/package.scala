package net.karana

package object unsigned {
  trait UByteIsIntegral extends Integral[UByte] {
    import UByte.toUByte
    private def div(x: UByte, y: UByte): (Int, Int) = {
      val xpos = (x.byte & (1 << 7)) == 0
      val ypos = (y.byte & (1 << 7)) == 0
      if (xpos && ypos) {
        val q = x.byte / y.byte
        (q, (x.byte - y.byte * q))
      } else if (xpos) { //y>x
        (0, x.toInt)
      } else if (ypos) { // x>y
        // x>= 128
        val rem = (64 % y.byte) << 1
        val q = (64 / y.byte) << 1
        val pos = x.byte & Byte.MaxValue
        val rr = pos % y.byte
        val carry = (rr + rem) / y.byte
        val qr = pos / y.byte
        (carry + qr + q, rr + rem - carry * y.byte)
      } else { // both larger that 127
        if (x >= y) (1, (x - y).byte)
        else (0, x.toInt)
      }
    }
    def quot(x: UByte, y: UByte): UByte = div(x, y)._1

    def rem(x: UByte, y: UByte): UByte = div(x, y)._2

    /**
     * As seen from class UByteIsIntegral, the missing signatures are as follows.
     *
     *
     *
     *  *  For convenience, these are usable as stub implementations.
     */
    // Members declared in scala.math.Numeric   
    def fromInt(x: Int): UByte = x.toByte
    def minus(x: UByte, y: UByte): UByte = (x.byte - y.byte).toByte
    def negate(x: UByte): UByte = (-x.byte).toByte
    def plus(x: UByte, y: UByte): UByte = (x.byte + y.byte).toByte
    def times(x: UByte, y: UByte): UByte = ((x.byte * y.byte) & 0xff).toByte
    def toDouble(x: UByte): Double = toInt(x)
    def toFloat(x: UByte): Float = toInt(x)
    def toInt(x: UByte): Int = if (x.byte < 0) 256 + x.byte else x.byte
    def toLong(x: UByte): Long = toInt(x)
    // Members declared in scala.math.Ordering   
    def compare(x: UByte, y: UByte): Int = if (x.byte == y.byte) 0 else {
      val same = ((x.byte & (1 << 7)) ^ (y.byte & (1 << 7))) == 0
      val lt = x.byte - y.byte < 0
      if (lt ^ same) 1 else -1
    }
  }

  implicit object UByteIsIntegral extends UByteIsIntegral

  trait UShortIsIntegral extends Integral[UShort] {
    import UShort.toUShort
    private def div(x: UShort, y: UShort): (Int, Int) = {
      val xpos = (x.short & (1 << 15)) == 0
      val ypos = (y.short & (1 << 15)) == 0
      if (xpos && ypos) {
        val q = x.short / y.short
        (q, (x.short - y.short * q))
      } else if (xpos) { //y>x
        (0, x.toInt)
      } else if (ypos) { // x>y
        // x>= 32768
        val rem = (16384 % y.short) << 1
        val q = (16384 / y.short) << 1
        val pos = x.short & Short.MaxValue
        val rr = pos % y.short
        val carry = (rr + rem) / y.short
        val qr = pos / y.short
        (carry + qr + q, rr + rem - carry * y.short)
      } else { // both larger that 32768
        val rem = (x - y).short
        if (rem < 0) (0, x.toInt)
        else (1, rem)
      }
    }

    def quot(x: UShort, y: UShort): UShort = div(x, y)._1.toShort
    def rem(x: UShort, y: UShort): UShort = div(x, y)._2.toShort

    /**
     * As seen from class UShortIsIntegral, the missing signatures are as follows.
     *
     *
     *
     *  *  For convenience, these are usable as stub implementations.
     */
    // Members declared in scala.math.Numeric   
    def fromInt(x: Int): UShort = x.toShort
    def minus(x: UShort, y: UShort): UShort = (x.short - y.short).toShort
    def negate(x: UShort): UShort = (-x.short).toShort
    def plus(x: UShort, y: UShort): UShort = (x.short + y.short).toShort
    def times(x: UShort, y: UShort): UShort = ((x.short * y.short) & 0xffff).toShort
    def toDouble(x: UShort): Double = toInt(x)
    def toFloat(x: UShort): Float = toInt(x)
    def toInt(x: UShort): Int = if (x.short < 0) 655356 + x.short else x.short
    def toLong(x: UShort): Long = toInt(x)
    // Members declared in scala.math.Ordering   
    def compare(x: UShort, y: UShort): Int = if (x.short == y.short) 0 else {
      val same = ((x.short & (1 << 15)) ^ (y.short & (1 << 15))) == 0
      val lt = x.short - y.short < 0
      if (lt ^ same) 1 else -1
    }
  }

  implicit object UShortIsIntegral extends UShortIsIntegral

  trait UIntIsIntegral extends Integral[UInt] {
    import UInt.toUInt

    private def div(x: UInt, y: UInt): (Int, Int) = {
      val xpos = (x.int & (1 << 31)) == 0
      val ypos = (y.int & (1 << 31)) == 0
      if (xpos && ypos) {
        val q = x.int / y.int
        (q, (x.int - y.int * q))
      } else if (xpos) { //y>x
        (0, x.toInt)
      } else if (ypos) { // x>y
        // x>= (1<<30)
        val (rem, q) = if (y.int >= (1 << 30)) {
          // y is a large positive, it will fit in 1<<63 one time
          // remainder is not so big
          val rem = -(y.int + (1 << 31))
          (rem, 1)
        } else {
          val rem = ((1 << 30) % y.int) << 1
          val q = ((1 << 30) / y.int) << 1
          (rem, q)
        }
        val pos = x.int & Int.MaxValue
        val rr = pos % y.int
        val carry = (rr + rem) / y.int
        val qr = pos / y.int
        (carry + qr + q, rr + rem - carry * y.int)
      } else { // both larger that (1<<30)
        val rem = (x - y).int
        if (rem < 0) (0, x.toInt)
        else (1, rem)
      }
    }

    def quot(x: UInt, y: UInt): UInt = div(x, y)._1
    def rem(x: UInt, y: UInt): UInt = div(x, y)._2

    /**
     * As seen from class UIntIsIntegral, the missing signatures are as follows.
     *
     *
     *
     *  *  For convenience, these are usable as stub implementations.
     */
    // Members declared in scala.math.Numeric   
    def fromInt(x: Int): UInt = x.toInt
    def minus(x: UInt, y: UInt): UInt = (x.int - y.int).toInt
    def negate(x: UInt): UInt = (-x.int).toInt
    def plus(x: UInt, y: UInt): UInt = (x.int + y.int).toInt
    def times(x: UInt, y: UInt): UInt = ((x.int * y.int) & -1).toInt
    def toDouble(x: UInt): Double = toLong(x)
    def toFloat(x: UInt): Float = toLong(x)
    def toInt(x: UInt): Int = x.int
    def toLong(x: UInt): Long = if (x.int < 0) (1l<<32)+x.int else x.int
    // Members declared in scala.math.Ordering   
    def compare(x: UInt, y: UInt): Int = if (x.int == y.int) 0 else {
      val xpos = (x.int & (1 << 31)) == 0
      val ypos = (y.int & (1 << 31)) == 0
      if (xpos) {
        if (ypos) { // both positive
          if (x.int - y.int < 0) -1 else 1
        } else -1
      } else if (ypos) {
        1
      } else { //both negative
        if (x.int - y.int < 0) -1 else 1
      }
    }
  }

  implicit object UIntIsIntegral extends UIntIsIntegral

  trait ULongIsIntegral extends Integral[ULong] {
    import ULong.toULong

    private def div(x: ULong, y: ULong): (Long, Long) = {
      val xpos = (x.long & (1l << 63)) == 0
      val ypos = (y.long & (1l << 63)) == 0
      if (xpos && ypos) {
        val q = x.long / y.long
        (q, (x.long - y.long * q))
      } else if (xpos) { //y>x
        (0, x.long)
      } else if (ypos) { // x>y
        // x>= (1<<62)
        val (rem, q) = if (y.long >= (1l << 62)) {
          // y is a large positive, it will fit in 1<<63 one time
          val q = 1l
          // remainder is not so big
          val rem = -(y.long + (1l << 63))
          (rem, q)
        } else {
          val rem = ((1l << 62) % y.long) << 1
          val q = ((1l << 62) / y.long) << 1
          (rem, q)
        }
        val pos = x.long & Long.MaxValue
        val rr = pos % y.long
        val carry = (rr + rem) / y.long
        val qr = pos / y.long
        (carry + qr + q, rr + rem - carry * y.long)

      } else { // both larger that (1<<62)
        val rem = (x - y).long
        if (rem < 0) (0, x.long)
        else (1, rem)
      }
    }

    def quot(x: ULong, y: ULong): ULong = div(x, y)._1
    def rem(x: ULong, y: ULong): ULong = div(x, y)._2

    /**
     * As seen from class ULongIsIntegral, the missing signatures are as follows.
     *
     *
     *
     *  *  For convenience, these are usable as stub implementations.
     */
    // Members declared in scala.math.Numeric   
    def fromInt(x: Int): ULong = UInt(x).toLong
    def minus(x: ULong, y: ULong): ULong = (x.long - y.long).toLong
    def negate(x: ULong): ULong = (-x.long).toLong
    def plus(x: ULong, y: ULong): ULong = (x.long + y.long).toLong
    def times(x: ULong, y: ULong): ULong = x.long * y.long
    def toDouble(x: ULong): Double = if (x.long < 0) java.lang.Long.MAX_VALUE * 2.0 + x.long else x.long.toDouble
    def toFloat(x: ULong): Float = if (x.long < 0) java.lang.Long.MAX_VALUE * 2.0f + x.long else x.long.toFloat
    def toInt(x: ULong): Int = x.long.toInt
    def toLong(x: ULong): Long = x.long
    // Members declared in scala.math.Ordering   
    def compare(x: ULong, y: ULong): Int = if (x.long == y.long) 0 else {
      val xpos = (x.long & (1l << 63)) == 0
      val ypos = (y.long & (1l << 63)) == 0
      if (xpos) {
        if (ypos) { // both positive
          if (x.long - y.long < 0) -1 else 1
        } else -1
      } else if (ypos) {
        1
      } else { //both negative
        if (x.long - y.long < 0) -1 else 1
      }
    }
  }

  implicit object ULongIsIntegral extends ULongIsIntegral

}