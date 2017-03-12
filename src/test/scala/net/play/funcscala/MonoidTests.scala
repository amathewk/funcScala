package net.play.funcscala

//import org.specs2.mutable._
import org.specs2._

class MonoidTests extends Specification {

  def is = s2"""
      test string concat monoid ${testStringMonoid}
      test int add monoid ${testIntAddMonoid}
      test int mult monoid ${testIntMultMonoid}
      test bool and monoid ${testBoolAndMonoid}
      test bool or monoid ${testBoolOrMonoid}
    """

  def testStringMonoid = {
    val s = new StringConcatMonoid
    val a = "abc"
    val b = "def"
    val c = "jlkd"
    isAssociative(s, a, b, c) and
      identityWorks(s, a)
  }

  def testIntAddMonoid = {
    val s = new IntAddMonoid
    val a = 3432
    val b = 762
    val c = 34
    isAssociative(s, a, b, c) and
      identityWorks(s, a)
  }

  def testIntMultMonoid = {
    val s = new IntMultMonoid
    val a = 3432
    val b = 762
    val c = 343
    isAssociative(s, a, b, c) and
      identityWorks(s, a)
  }

  def testBoolAndMonoid = {
    val s = new BoolAndMonoid
    val a = true
    val b = false
    val c = true
    isAssociative(s, a, b, c) and
      identityWorks(s, a)
  }

  def testBoolOrMonoid = {
    val s = new BoolOrMonoid
    val a = false
    val b = true
    val c = false
    isAssociative(s, a, b, c) and
      identityWorks(s, a)
  }

  private def isAssociative[T](m:Monoid[T], a:T, b:T, c:T) = m.op(a, m.op(b, c)) == m.op(a, m.op(b,c))

  private def identityWorks[T](m:Monoid[T], a:T) = (m.op(a, m.zero) == a) && (m.op(m.zero, a) == a)

  private def do3a[T](m : Monoid[T], a:T, b:T, c:T ) = m.op(m.op(a,b), c)

  private def do3b[T](m : Monoid[T], a:T, b:T, c:T ) = m.op(a, m.op(b,c))

}