object Exercise6 extends App {

  trait ConsInterface[+A] {
    def value: A
    def next: ConsInterface[A]

    def length(): Int
    def last(): A
    def add[B >: A](addValue: B): ConsInterface[B] = Cons(addValue, this)
    def remove(): ConsInterface[A] = next
    def reverse(): ConsInterface[A] = {
      val r = NilCons.add(value)
    }
  }

  case class Cons[+A] (val value: A, val next: ConsInterface[A]) extends ConsInterface[A] {
    override def toString = s"head: $value, next: $next"

    override def length(): Int = 1 + next.length

    override def last(): A = next match {
      case NilCons => value
      case _ => next.last()
    }

//    override def add[B >: A](addValue: B): ConsInterface[B] = super.add(addValue)
  }

  object NilCons extends ConsInterface[Nothing] {
    def value = throw new NoSuchElementException("head of empty list")
    def next = throw new UnsupportedOperationException("tail of empty list")

    override def length(): Int = 0

    override def last(): Nothing = throw new NoSuchElementException("head of empty list")

    //    override def add[B >: Nothing](addValue: B): ConsInterface[B] = super.add(addValue)
    override def remove(): ConsInterface[Nothing] = throw new UnsupportedOperationException("tail of empty list")
  }


    val c1 = NilCons.add(1)
    val c2 = c1.add(2)
    val c3 = c2.add(3)
    val c4 = c3.add(4)
    val n3 = c4.remove()
    println(c1)
    println(c2)
    println(c3)
    println(c4)
    print(n3)

    println(c4.length())

    println(c4.last())


}
