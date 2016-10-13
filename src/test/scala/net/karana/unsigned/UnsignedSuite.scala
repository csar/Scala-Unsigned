package net.karana.unsigned

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.util.Random

@RunWith(classOf[JUnitRunner])
class UnsignedSuite extends FunSuite {
  val multiplier=1
  test("addUByte") {
     val sum = UByte(100)+UByte(100)+UByte(50)

      assert(sum.toInt === 250)
     val s2 = UByte(10)+200
     assert(s2.toInt === 210)
     val s3 = UByte(100)+200
     assert(s3.toInt === 44)
  }

  test("substractUB") {
   val sum = UByte(100)+UByte(100)-UByte(50)
      assert(sum.toInt === 150)
    val pos = (UByte(200.toByte)) -190
    assert(pos.toInt === 10)
  }

  test("substractShort") {
   for( c<-0 to 10000000) {
     val i = Random.nextInt(65536)
     val j = Random.nextInt(65536)
     val u1 = UShort(i.toShort)     
     val u2 = UShort(j.toShort)     
     val r = (u1-u2).toInt
     val t = i-j
     val tr =  if (t<0) {
       65536+t
     } else {
       t & ((1<<16)-1)
     }
      assert(tr === r)
  }
  }
  test("substractInt") {
   for( c<-0 to 10000000) {
     val i = Random.nextInt()
     val j = Random.nextInt()
     val u1 = UInt(i)     
     val u2 = UInt(j)     
     val r = (u1-u2).toLong
     val t:Long = i-j
     val tr =  if (t<0) {
       (1l<<32)+t
     } else {
       t & ((1l<<32)-1)
     }
      assert(tr === r)
  }
  }
  test("negateLong") {
   for( c<-0 to 10000000) {
     val i = Random.nextLong()
     val j = Random.nextLong()
     val u1 = ULong(i)     
     val u2 = ULong(j)     
     val r = (u1-u2).toBigInt
     val tr =  (u1 + (-u2)).toBigInt
      assert(tr === r)
  }
  }
  test("negateInt") {
   for( c<-0 to 10000000) {
     val i = Random.nextInt()
     val j = Random.nextInt()
     val u1 = UInt(i)     
     val u2 = UInt(j)     
     val r = (u1-u2).toLong
     val tr =  (u1 + (-u2)).toLong
      assert(tr === r)
  }
  }
  test("negateShort") {
   for( c<-0 to 10000000) {
     val i = Random.nextInt(65536)
     val j = Random.nextInt(65536)
     val u1 = UShort(i.toShort)     
     val u2 = UShort(j.toShort)     
     val s = (u1-u2)
     val a =  (u1 + (-u2))
      assert(s === a, "eqUShort")
      assert(s.toInt === a.toInt)
  }
  }
  test("negateByte") {
   for( i<-0 to 255;j<-0 to 255){
     val u1 = UShort(i.toShort)     
     val u2 = UShort(j.toShort)     
     val s = (u1-u2)
     val a =  (u1 + (-u2))
      assert(s === a, "eqUShort")
      assert(s.toInt === a.toInt)
  }
  }

  test("substractLong") {
   for( c<-0 to 10000000) {
     val i = Random.nextLong()
     val j = Random.nextLong()
     val u1 = ULong(i)     
     val u2 = ULong(j)     
     val r = (u1-u2).toBigInt
     val t = u1.toBigInt-u2.toBigInt
     val tr =  if (t<0) {
       (BigInt(1)<<64)+t
     } else {
       t & ((BigInt(1)<<64)-1)
     }
      assert(tr === r)
  }
  }

  test("compare bytes") {
   for( i<-0 to 255; j<-0 to 255) {
     val u1:UByte = i
     val u2:UByte = j
     assert(u1.compare(u1, u2) === i.compare(j),s"$i and $j")
    }
  }
  test("divide bytes") {
    UByte(-128)/1
   for( i<-0 to 255; j<-1 to 255) {
     val u1:UByte = i
     val u2:UByte = j
     assert( (u1/u2).toInt === i/j,s"$i and $j")
    }
  }
  test("remainder bytes") {
   for( i<-0 to 255; j<-1 to 255) {
     val u1:UByte = i
     val u2:UByte = j
     assert( (u1%u2).toInt === i%j,s"$i and $j")
    }
  }
  test("divide shorts") {
    UShort(53485.toShort)/10242.toShort
   for( c<-0 to 1000000*multiplier) {
     val i = Random.nextInt(65536)
     val j = Random.nextInt(65535)+1
     val u1:UShort = i.toShort
     val u2:UShort = j.toShort
     assert( (u1/u2).toInt === i/j,s"$i / $j")
     assert( (u1%u2).toInt === i%j,s"$i % $j")
    }
  }
  test("divide longs") {
    ULong(-7942979444136742363l)/ULong(8655271714020611535l)
   for( c<- 0 to 1000000*multiplier) {
     val i = Random.nextLong
     var j = 0l
     while(j==0  ) j =Random.nextLong
     
     val u1:ULong = i
     val u2:ULong = j
     assert( (u1/u2).toBigInt === u1.toBigInt/u2.toBigInt,s"$u1 / $u2 $i $j")
     assert( (u1%u2).toBigInt === u1.toBigInt%u2.toBigInt,s"$u1 % $u2 $i $j")
    }
  }
  test("multiply ints") {
   val res =  UInt(-320121116  ) /UInt(783123095)
   for( c<- 0 to 1000000*multiplier) {
     val i =  Random.nextInt
     var j = 0
     while(j==0 ) j =Random.nextInt
     
     val u1:UInt = i
     val u2:UInt = j
     assert( (u1*u2).toLong === ((u1.toLong*u2.toLong)&((1l<<32))-1),s"$u1 * $u2 $i $j")
    }
  }
  test("multiply longs") {
   val res =  ULong(-1334168375376823671L  ) * ULong(-5855111396027730172L)
   for( c<- 0 to 1000000*multiplier) {
     val i =  Random.nextLong
     val j = Random.nextLong
     
     val u1:ULong = i
     val u2:ULong = j
     assert( (u1*u2).toBigInt === ((u1.toBigInt*u2.toBigInt)&((BigInt(1)<<64))-1),s"$u1 * $u2 $i $j")
    }
  }
  test("multiply shorts") {
   for( c<- 0 to 1000000*multiplier) {
     val i =  Random.nextInt(65536)
     val j = Random.nextInt(65536)
     
     val u1:UShort = i.toShort
     val u2:UShort = j.toShort
     assert( (u1*u2).toInt === ((u1.toInt*u2.toInt)&((1<<16))-1),s"$u1 * $u2 $i $j")
    }
  }
  test("multiply bytes") {
   for( i<- 0 to 255 ; j<-0 to 255) {
     
     val u1:UByte = i
     val u2:UByte = j
     assert( (u1*u2).toInt === ((i*j) & 0xff),s"$u1 * $u2 $i $j")
    }
  }
  test("divide ints") {
   val res =  UInt(-320121116  ) /UInt(783123095)
   for( c<- 0 to 1000000*multiplier) {
     val i =  Random.nextInt
     var j = 0
     while(j==0 ) j =Random.nextInt
     
     val u1:UInt = i
     val u2:UInt = j
     assert( (u1/u2).toLong === u1.toLong/u2.toLong,s"$u1 / $u2 $i $j")
     assert( (u1%u2).toLong === u1.toLong%u2.toLong,s"$u1 % $u2 $i $j")
    }
  }
  test("compare shorts") {
    
   for( c<-0 to 10000000) {
     val i = Random.nextInt(65536)
     val j = Random.nextInt(65536)
     val u1:UShort = i.toShort
     val u2:UShort = j.toShort
     assert(u1.compare(u1, u2) === i.compare(j),s"$i and $j")
    }
  }
  test("compare ints") {
   val k  = UInt(2065289431)<UInt(-614927737)
   for( c<-0 to 10000000) {
     val u1=UInt(Random.nextInt())
     val u2=UInt(Random.nextInt())
     val i=u1.toLong
     val j=u2.toLong
     assert(u1.compare(u1, u2) === i.compare(j),s"$i and $j")
    }
  }
  test("compare longs") {
    val k = ULong(-676066946352325297l) < ULong(7955596228505083520l)
    assert(k===false)
   for( c<-0 to 10000000) {
     val u1=ULong(Random.nextLong())
     val u2=ULong(Random.nextLong())
     val i=u1.toBigInt
     val j=u2.toBigInt
     assert(u1.compare(u1, u2) === i.compare(j),s"$i and $j")
    }
  }
//
//  test("union: set4c and set4d") {
//    new TestSets {
//      assert(size(set4c.union(set4d)) === 4)
//    }
//  }
//
//  test("union: with empty set (1)") {
//    new TestSets {
//      assert(size(set5.union(set1)) === 4)
//    }
//  }
//
//  test("union: with empty set (2)") {
//    new TestSets {
//      assert(size(set1.union(set5)) === 4)
//    }
//  }
//
//  test("descending: set5") {
//    new TestSets {
//      val trends = set5.descendingByRetweet
//      assert(!trends.isEmpty)
//      assert(trends.head.user == "a" || trends.head.user == "b")
//    }
//  }

  }
