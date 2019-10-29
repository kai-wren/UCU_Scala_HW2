object Exercise6 extends App {

  trait lListInterface[+A] {
    def value: A
    def ref: lListInterface[A]

    def length(): Int
    def last(): A
    def addLeft[B >: A](addValue: B): lListInterface[B] = lList(addValue, this)
    def removeLeft(): lListInterface[A] = ref
    def reverse[B >: A](list: lListInterface[B] = NilList): lListInterface[B] = list match {
        case _ if (ref == NilList) => list.addLeft(value)
        case _ => ref.reverse(list.addLeft(value))
      }
    def addRight[B >: A](addValue: B): lListInterface[B] = this match {
      case NilList => NilList.addLeft(addValue)
      case _ => ref.addRight(addValue).addLeft(value)
    }

    def removeRight(): lListInterface[A] = this match {
      case _ if (ref.ref == NilList) =>  NilList.addLeft(value)
      case _ => ref.removeRight().addLeft(value)
    }

  }

  case class lList[+A] (val value: A, val ref: lListInterface[A]) extends lListInterface[A] {

    override def toString = "value: " + value + " ref: " + ref

    override def length(): Int = 1 + ref.length()

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

    override def removeLeft(): lListInterface[Nothing] = throw new UnsupportedOperationException("Nothing to remove")

    override def reverse[B >: Nothing](list: lListInterface[B]): lListInterface[B] = throw new UnsupportedOperationException("Nothing to reverse")

    override def removeRight(): lListInterface[Nothing] = throw new UnsupportedOperationException("Nothing to remove")
  }


    //checking result
    val l5 = NilList.addLeft(1).addRight(2).addLeft(3).addRight(4).addLeft(5)
    val l4r = l5.removeRight()
    val l4l = l5.removeLeft()
    val l6r = l5.addRight(6)
    val l6l = l5.addLeft(6)
    val rev5 = l5.reverse()

    //printing lists
    println("l5 - " + l5)
    println("l4r - " + l4r)
    println("l4l - " + l4l)
    println("rev5 - " + rev5)
    println("l6r - " + l6r)
    println("l6l - " + l6l)

    //check length
    assert(l5.length() == 5)
    assert(l4r.length() == 4)
    assert(l4l.length() == 4)
    assert(l6r.length() == 6)
    assert(l6l.length() == 6)
    assert(rev5.length() == 5)

    //check last element of different lists to check other methods
    assert(l5.last() == 4)
    assert(l4r.last() == 2)
    assert(l4l.last() == 4)
    assert(rev5.last() == 5)
    assert(l6r.last() == 6)
    assert(l6l.last() == 4)



}
