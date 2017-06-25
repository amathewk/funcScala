package net.play.funcscala

object Monoids2 {

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = as.map(f).foldLeft(m.zero)(m.op)

  def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    val minSize = 2
    if (v.size <= minSize ) return v.map(f).foldLeft(m.zero)(m.op)
    val (first, second) = v.splitAt(v.size/2)
    m.op(foldMapV(first, m)(f),foldMapV(second, m)(f))
  }

  val wcMonoid : Monoid[WC] = new Monoid[WC] {
    override def op(a1: WC, a2: WC): WC = {
      (a1,a2) match {
        case (Stub(x), Stub(y)) => Stub(x+y)
        case (Part(l,count,r), Stub(y)) => Part(l,count,r+y)
        case (Part(l1,count1,r1), Part(l2,count2,r2)) => Part(l1,count1+count2+1,r2)
        case (Stub(x), Part(l,count,r)) => Part(x+l,count,r)
      }
    }

    override def zero: WC = Stub("")
  }
}