object Exercise4 extends App {
class Rational (n: BigInt, d: BigInt) {
  def numer: BigInt = n
  def denom: BigInt = d

  //addition
  def add(R: Rational): Rational = {
    val nr = n*R.denom+R.numer*d
    val dr = d*R.denom
    val r = Rational(nr/nr.gcd(dr), dr/nr.gcd(dr))
    r.get
  }

  //subtraction
  def sub(R: Rational): Rational = {
    val nr = n*R.denom-R.numer*d
    val dr = d*R.denom
    val r = Rational(nr/nr.gcd(dr), dr/nr.gcd(dr))
    r.get
  }

  //multiplication
  def mul(R: Rational): Rational = {
    val nr = n*R.numer
    val dr = d*R.denom
    val r = Rational(nr/nr.gcd(dr), dr/nr.gcd(dr))
    r.get
  }

  //division
  def div(R: Rational): Rational = {
    val nr = n*R.denom
    val dr = d*R.numer
    val r = Rational(nr/nr.gcd(dr), dr/nr.gcd(dr))
    r.get
  }

  // check for equality
  def equal(R: Rational): Boolean = {
    if (n/n.gcd(d) == R.numer/R.numer.gcd(R.denom) && d/n.gcd(d) == R.denom/R.numer.gcd(R.denom)) true
    else false
  }
}

  //companion object
  object Rational {
    def apply(n: BigInt, d: BigInt): Option[Rational] = {
      val R = new Rational(n, d)
      if (d == 0) None
      else Some(R)
    }
  }

  //checking result
  try{
    val r1 = Rational(1, 5).get
    val r2 = Rational(1, 3).get
    val r3 = Rational(1, 4).get
    val r4 = Rational(1, 8).get
    val r5 = Rational(1, 8).get
    val r6 = Rational(2, 16).get

    val ra1 = r1.add(r2)
    val rs1 = r1.sub(r2)
    val rm1 = r1.mul(r2)
    val rd1 = r1.div(r2)

    val ra2 = r3.add(r4)
    val rs2 = r3.sub(r4)
    val rm2 = r3.mul(r4)
    val rd2 = r3.div(r4)

    assert(ra1.numer == 8 && ra1.denom == 15)
    assert(rs1.numer == -2 && rs1.denom == 15)
    assert(rm1.numer == 1 && rm1.denom == 15)
    assert(rd1.numer == 3 && rd1.denom == 5)
    assert(!r1.equal(r2))

    assert(ra2.numer == 3 && ra2.denom == 8)
    assert(rs2.numer == 1 && rs2.denom == 8)
    assert(rm2.numer == 1 && rm2.denom == 32)
    assert(rd2.numer == 2 && rd2.denom == 1)
    assert(!r3.equal(r4))

    assert(r4.equal(r5))
    assert(r5.equal(r6))
  }
  catch {
    case assertionError: AssertionError => println("Assertion failed!")
    case _: Throwable => println("Not possible to create Rational with such parameters!")
  }

}
