object Exercise6 extends App {

  trait lListInterface[+A] {
    def value: A
    def ref: lListInterface[A]

    def length(): Int
    def last(): A
    def addLeft[B >: A](addValue: B): lListInterface[B] = lList(addValue, this)
    def remove(): lListInterface[A] = ref
    def reverse[B >: A](list: lListInterface[B] = NilList): lListInterface[B] = list match {
        case NilList => ref.reverse(list.addLeft(value))
        case _ if (ref == NilList) => list.addLeft(value)
        case _ => ref.reverse(list.addLeft(value))
      }
    def addRight[B >: A](addValue: B, list: lListInterface[B] = this): lListInterface[B] = list match {
//      case NilList => list
      case _ if (ref == NilList) => list
//      case _ => ref.addRight(addValue, list.addLeft(value))
      case _ => list.addLeft(value).addRight(addValue, list.ref)
    }

  }

  case class lList[+A] (val value: A, val ref: lListInterface[A]) extends lListInterface[A] {

    override def toString = s"head: $value, next: $ref"

    override def length(): Int = 1 + ref.length

    override def last(): A = ref match {
      case NilList => value
      case _ => ref.last()
    }

  }

  object NilList extends lListInterface[Nothing] {

    def value = throw new NoSuchElementException("No value")

    def ref = throw new UnsupportedOperationException("No reference")

    override def length(): Int = 0

    override def last(): Nothing = throw new NoSuchElementException("No value")

    override def remove(): lListInterface[Nothing] = throw new UnsupportedOperationException("No reference")

    override def reverse[B >: Nothing](list: lListInterface[B]): lListInterface[B] = throw new UnsupportedOperationException("No reference")
  }


    val c1 = NilList.addLeft(1)
    val c2 = c1.addLeft(2)
    val c3 = c2.addLeft(3)
    val c4 = c3.addLeft(4)
    val n3 = c4.remove()
    val r4 = c4.reverse()
    val c5 = c4.addRight(5)
    println(c1)
    println(c2)
    println(c3)
    println(c4)
    println(n3)
    println(r4)
    println(c5)


    assert(c4.length() == 4)

    assert(c4.last() == 1)
    assert(r4.last() == 4)


}
