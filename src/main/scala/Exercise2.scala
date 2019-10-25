object Exercise2 extends App {

  val gFunc = (num: Int) => scala.math.pow(num, 4)
  val fFunc = (num: Double) => scala.math.sqrt(num)

  def compose[A,B, C] (f: B=>C, g: A=>B): A=>C = g.andThen(f)

  //checking result
  assert(compose(fFunc, gFunc)(2) == 4)
  assert(compose(fFunc, gFunc)(3) == 9)
  assert(compose(fFunc, gFunc)(4) == 16)
}
