package net.play.funcscala

//import org.specs2.mutable._
import org.specs2._

class Monoids1Tests extends Specification {

  def is = s2"""
      test string concat monoid ${testStringMonoid}
      test int add monoid ${testIntAddMonoid}
      test int mult monoid ${testIntMultMonoid}
      test bool and monoid ${testBoolAndMonoid}
      test bool or monoid ${testBoolOrMonoid}
      test option monoid ${testOptionMonoid}
      test endoid monoid ${testEndoidMonoid}
    """

  def testStringMonoid = {
    val s = new StringConcatMonoid
    val a = "abc"
    val b = "def"
    val c = "jlkd"
    MonoidTest.isAssociative(s, a, b, c) and
      MonoidTest.identityWorks(s, a)
  }

  def testIntAddMonoid = {
    val s = new IntAddMonoid
    val a = 3432
    val b = 762
    val c = 34
    MonoidTest.isAssociative(s, a, b, c) and
      MonoidTest.identityWorks(s, a)
  }

  def testIntMultMonoid = {
    val s = new IntMultMonoid
    val a = 3432
    val b = 762
    val c = 343
    MonoidTest.isAssociative(s, a, b, c) and
      MonoidTest.identityWorks(s, a)
  }

  def testBoolAndMonoid = {
    val s = new BoolAndMonoid
    val a = true
    val b = false
    val c = true
    MonoidTest.isAssociative(s, a, b, c) and
      MonoidTest.identityWorks(s, a)
  }

  def testBoolOrMonoid = {
    val s = new BoolOrMonoid
    val a = false
    val b = true
    val c = false
    MonoidTest.isAssociative(s, a, b, c) and
      MonoidTest.identityWorks(s, a)
  }

  def testOptionMonoid = {
    val o = new OptionMonoid[Int]
    val a = Some(3)
    val b = None
    val c = Some(2)
    MonoidTest.isAssociative(o, a, b, c) and
      MonoidTest.identityWorks(o, a) and
      MonoidTest.identityWorks(o, b)
  }

  def testEndoidMonoid = {
    val e = new EndoidMonoid[Int]
    val a = {i:Int => i+5}
    val b = {i:Int => 25}
    val c = {i:Int => i*9}
    e.op(a, e.op(b,c))(100) must_== e.op(e.op(a,b), c)(100)
    e.op(a, e.zero)(200) must_== a(200)
    e.op(e.zero, a)(200) must_== a(200)
    e.op(e.zero, b)(200) must_== b(200)
    e.op(b, e.zero)(200) must_== b(200)

    val ints = Seq(2,5,1,99)
    true
//    Monoids2.foldMap(ints, new StringConcatMonoid)(anInt => anInt.toString)

  }

  private def do3a[T](m : Monoid[T], a:T, b:T, c:T ) = m.op(m.op(a,b), c)

  private def do3b[T](m : Monoid[T], a:T, b:T, c:T ) = m.op(a, m.op(b,c))


}

object MonoidTest {
  def isAssociative[T](m:Monoid[T], a:T, b:T, c:T) = m.op(a, m.op(b, c)) == m.op(a, m.op(b,c))
  def identityWorks[T](m:Monoid[T], a:T) = (m.op(a, m.zero) == a) && (m.op(m.zero, a) == a)
}