object Exercise1 extends App {

  val printFunc = println("I am zero-argument function!!!")
  val sumFunc = 1+1
  val sqr2Func = scala.math.pow(2, 2)

  def execute[A] (func: A):A = func

  //checking result
  execute(printFunc)

  assert(execute(sumFunc) == 2)
  assert(execute(sqr2Func) == 4)

}
