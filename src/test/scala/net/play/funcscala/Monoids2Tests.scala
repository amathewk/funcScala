package net.play.funcscala

import org.specs2.mutable._

class Monoids2Tests extends Specification {

  "Monoid tests 2".txt

  "foldMap test" >> {
    "foldMap from Int to String" >> {
      val nums = List(2,5,1,99)
      Monoids2.foldMap(nums, new StringConcatMonoid)(anInt => anInt.toString) === "25199"
    }
  }

  "foldMapV test" >> {
    val nums = IndexedSeq(2,5,1,99)
    Monoids2.foldMapV(nums, new StringConcatMonoid)(anInt => anInt.toString) === "25199"
  }

  "WC monoid" >> {
    val wcMonoid = Monoids2.wcMonoid
//    "lorem ipsum dolor sit amet, "
    val wc1 = Part("lorem", 1, "do")
    val wc2 = Part("lor", 1, "a")
    val wc3 = Stub("met")

    import wcMonoid._
    op( op(wc1, wc2), wc3) === Part("lorem", 3, "amet")
    List(wc1, wc2, wc3).foldLeft(true){(result, wc) => result && MonoidTest.identityWorks(wcMonoid, wc) }
    MonoidTest.isAssociative(wcMonoid, wc1, wc2, wc3)
  }

//  "word count using WC Monoid " >> {
//    val sentence = "lorem ipsum dolor sit amet"
//    Mondoids2.wordCount(sentence) === 5
//  }

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