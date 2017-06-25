package net.play.funcscala

trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero: A
}

class StringConcatMonoid extends Monoid[String] {
  def op(a1: String, a2: String) = a1 + a2

  val zero = ""
}

class IntAddMonoid extends Monoid[Int] {
  override def op(a1: Int, a2: Int): Int = a1 + a2

  override def zero: Int = 0
}

class IntMultMonoid extends Monoid[Int] {
  override def op(a1: Int, a2: Int): Int = a1 * a2

  override def zero: Int = 1
}

class BoolAndMonoid extends Monoid[Boolean] {
  override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

  override def zero: Boolean = true
}

class BoolOrMonoid extends Monoid[Boolean] {
  override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

  override def zero: Boolean = false
}

class OptionMonoid[A] extends Monoid[Option[A]] {
  override def op(a1: Option[A], a2: Option[A]): Option[A] = if (a1.isDefined) a1 else a2

  override def zero: Option[A] = None

}

class EndoidMonoid[A] extends Monoid[A => A] {
  override def op(a1: (A) => A, a2: (A) => A) = a1 compose a2

  override def zero = {i:A=>i}
}