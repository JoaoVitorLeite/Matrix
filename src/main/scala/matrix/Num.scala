package matrix

import scala.language.postfixOps

trait Num[A]{
  def plus (x:A, y:A): A
  def sub(x:A, y:A): A
  def mul(x:A, y:A): A
  def div(x:A, y:A): A
  def rm(x:A, y:A): A
  def zero: A
  def one: A
  def neg(x:A): A
  def sqroot(x:A): A
  def abs(x:A): A
  def pow(x:A, y:A): A
  def ltn(x:A, y:A): Boolean
  def ltneq(x:A, y:A): Boolean
  def gtn(x:A, y:A): Boolean
  def gtneq(x:A, y:A): Boolean
  def fact(x:A): A
  def intValue(x: A): Int
  def longValue(x: A): Long
  def doubleValue(x: A): Double

  class Ops(lhs: A){

    def + (rhs: A): A = plus(lhs, rhs)
    def unary_- : A = neg(lhs)
    def - (rhs: A): A = sub(lhs, rhs)
    def * (rhs: A): A = mul(lhs, rhs)
    def / (rhs: A): A = div(lhs, rhs)
    def % (rhs: A): A = rm(lhs, rhs)
    def < (rhs: A): Boolean = ltn(lhs, rhs)
    def <= (rhs: A): Boolean = ltneq(lhs, rhs)
    def > (rhs: A): Boolean = gtn(lhs, rhs)
    def >= (rhs: A): Boolean = gtneq(lhs, rhs)
    def ! : A = fact(lhs)
    def unary_! : A = fact(lhs)
    def toINT: Int = intValue(lhs)
    def toLONG: Long = longValue(lhs)
    def toDOUBLE: Double = doubleValue(lhs)
  }

  implicit def mkNumOps(lhs: A): Ops = new Ops(lhs)

}


object Num {

  trait ExtraImplicits{

    implicit def infixNumOps[A](x:A)(implicit num: Num[A]): Num[A]#Ops = new num.Ops(x)

  }

  object Implicits extends ExtraImplicits { }

  implicit object IntNum extends Num[Int]{
    override def plus(x: Int, y: Int): Int = x+y

    override def sub(x: Int, y: Int): Int = x-y

    override def mul(x: Int, y: Int): Int = x*y

    override def div(x: Int, y: Int): Int = x/y

    override def zero: Int = 0

    override def one: Int = 1

    override def rm(x: Int, y: Int): Int = x%y

    override def neg(x: Int): Int = -x

    override def sqroot(x: Int): Int = math.sqrt(x).toInt

    override def abs(x: Int): Int = if(x < 0) -x else x

    override def pow(x: Int, y: Int): Int = math.pow(x, y).toInt

    override def ltn(x: Int, y: Int): Boolean = if(x < y) true else false

    override def ltneq(x: Int, y: Int): Boolean = if(x <= y) true else false

    override def gtn(x: Int, y: Int): Boolean = if(x > y) true else false

    override def gtneq(x: Int, y: Int): Boolean = if(x >= y) true else false

    override def fact(x: Int): Int = {
      @scala.annotation.tailrec
      def loop(n: Int, acc: Int): Int ={
        if(n == 1) acc
        else loop(n-1, acc*n)
      }
      loop(x, 1)
    }

    override def intValue(x: Int): Int = x

    override def longValue(x: Int): Long = x.toLong

    override def doubleValue(x: Int): Double = x.toDouble
  }

  implicit object FloatNum extends Num[Float]{
    override def plus(x: Float, y: Float): Float = x+y

    override def sub(x: Float, y: Float): Float = x-y

    override def mul(x: Float, y: Float): Float = x*y

    override def div(x: Float, y: Float): Float = x/y

    override def zero: Float = 0f

    override def one: Float = 1f

    override def rm(x: Float, y: Float): Float = x%y

    override def neg(x: Float): Float = -x

    override def sqroot(x: Float): Float = math.sqrt(x).toFloat

    override def abs(x: Float): Float = if(x < 0) -x else x

    override def pow(x: Float, y: Float): Float = math.pow(x, y).toFloat

    override def ltn(x: Float, y: Float): Boolean = if(x < y) true else false

    override def ltneq(x: Float, y: Float): Boolean = if(x <= y) true else false

    override def gtn(x: Float, y: Float): Boolean = if(x > y) true else false

    override def gtneq(x: Float, y: Float): Boolean = if(x >= y) true else false

    override def fact(x: Float): Float = {
      @scala.annotation.tailrec
      def loop(n: Float, acc: Float): Float ={
        if(n == 1f) acc
        else loop(n-1f, acc*n)
      }
      loop(x, 1f)
    }

    override def intValue(x: Float): Int = x.toInt

    override def longValue(x: Float): Long = x.toLong

    override def doubleValue(x: Float): Double = x.toDouble
  }

  implicit object DoubleNum extends Num[Double]{
    override def plus(x: Double, y: Double): Double = x+y

    override def sub(x: Double, y: Double): Double = x-y

    override def mul(x: Double, y: Double): Double = x*y

    override def div(x: Double, y: Double): Double = x/y

    override def zero: Double = 0D

    override def one: Double = 1D

    override def rm(x: Double, y: Double): Double = x%y

    override def neg(x: Double): Double = -x

    override def sqroot(x: Double): Double = math.sqrt(x)

    override def abs(x: Double): Double = if(x < 0) -x else x

    override def pow(x: Double, y: Double): Double = math.pow(x,y)

    override def ltn(x: Double, y: Double): Boolean = if(x < y) true else false

    override def ltneq(x: Double, y: Double): Boolean = if(x <= y) true else false

    override def gtn(x: Double, y: Double): Boolean = if(x > y) true else false

    override def gtneq(x: Double, y: Double): Boolean = if(x >= y) true else false

    override def fact(x: Double): Double = {
      @scala.annotation.tailrec
      def loop(n: Double, acc: Double): Double ={
        if(n == 1D) acc
        else loop(n-1D, acc*n)
      }
      loop(x, 1D)
    }

    override def intValue(x: Double): Int = x.toInt

    override def longValue(x: Double): Long = x.toLong

    override def doubleValue(x: Double): Double = x
  }

  implicit object LongNum extends Num[Long]{
    override def plus(x: Long, y: Long): Long = x+y

    override def sub(x: Long, y: Long): Long = x-y

    override def mul(x: Long, y: Long): Long = x*y

    override def div(x: Long, y: Long): Long = x/y

    override def zero: Long = 0L

    override def one: Long = 1L

    override def rm(x: Long, y: Long): Long = x%y

    override def neg(x: Long): Long = -x

    override def sqroot(x: Long): Long = math.sqrt(x.toDouble).toLong

    override def abs(x: Long): Long = if(x < 0) -x else x

    override def pow(x: Long, y: Long): Long = math.pow(x,y).toLong

    override def ltn(x: Long, y: Long): Boolean = if(x < y) true else false

    override def ltneq(x: Long, y: Long): Boolean = if(x <= y) true else false

    override def gtn(x: Long, y: Long): Boolean = if(x > y) true else false

    override def gtneq(x: Long, y: Long): Boolean = if(x >= y) true else false

    override def fact(x: Long): Long = {
      @scala.annotation.tailrec
      def loop(n: Long, acc: Long): Long ={
        if(n == 1L) acc
        else loop(n-1L, acc*n)
      }
      loop(x, 1L)
    }

    override def intValue(x: Long): Int = x.toInt

    override def longValue(x: Long): Long = x

    override def doubleValue(x: Long): Double = x.toDouble
  }

}