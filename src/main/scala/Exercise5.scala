object Exercise5 extends App {

  trait Shape{
    def Perimeter: Either[Double, String]
    def Area: Either[Double, String]
  }

  class Rectangle (val points: List[(Double, Double)]) extends Shape{
    def getPoints: Option[(Double, Double, Double, Double)] = {
      points match {
        case (x1, y1) :: (x2, y2) :: Nil => Some((x1, y1, x2, y2))
        case _ => None
      }
    }
    override def Perimeter: Either[Double, String] = {
      if (getPoints.isDefined) {
        val (x1, y1, x2, y2) = getPoints.get
        Left(scala.math.abs(x2-x1)*2 + scala.math.abs(y2-y1)*2)
      }
      else Right("No such rectangle possible!")
    }

    override def Area: Either[Double, String] = {
      if (getPoints.isDefined) {
        val (x1, y1, x2, y2) = getPoints.get
        Left(scala.math.abs(x2-x1)*scala.math.abs(y2-y1))
      }
      else Right("No such rectangle possible!")
    }
  }

  // checking result
  val R = new Rectangle(List((-3,-3), (3,3)))
  R.Perimeter match {
    case Left(d) => assert(d == 24)
    case Right(s) => println(s)
  }
  R.Area match {
    case Left(d) => assert(d == 36)
    case Right(s) => println(s)
  }

  class Circle (val center: (Double, Double) = (0,0), val radius: Double) extends Shape {

    override def Perimeter: Either[Double, String] = {
       radius match {
         case _ if (radius > 0) => Left(2 * radius * scala.math.Pi)
         case _ => Right("No such circle exist!")
       }
    }

    override def Area: Either[Double, String] = {
      radius match {
        case _ if (radius > 0) => Left(scala.math.pow(radius, 2) * scala.math.Pi)
        case _ => Right("No such circle exist!")
      }
    }
  }

  // checking result
  val C = new Circle((1,1), 3)
  C.Perimeter match {
    case Left(d) => assert( d.round == 19)
    case Right(s) => println(s)
  }
  C.Area match {
    case Left(d) => assert( d.round == 28)
    case Right(s) => println(s)
  }
}
