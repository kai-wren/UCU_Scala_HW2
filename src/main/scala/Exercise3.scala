object Exercise3 extends App{
val iterFunc = (num: Int) => num + 1

  def callFuncNTimes[A, B <: Int] (f: A=>A, n: B): A=>A = {
    if (n > 1) f.compose(callFuncNTimes(f, n-1)) else f
  }

  //checking results
  assert(callFuncNTimes(iterFunc, 5)(1) == 6)
  assert(callFuncNTimes(iterFunc, 10)(1) == 11)
  assert(callFuncNTimes(iterFunc, 10)(5) == 15)
}
