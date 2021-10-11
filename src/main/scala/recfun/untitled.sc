def product(f: Int => Int)(a: Int, b:Int):Int =
  if a>b then 1 else f(a) * product(f)(a+1, b)


product(x => x*x)(1,5)

def fact(n:Int) = product(x => x)(1, n)

fact(5)

def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int =
  def recur(a: Int): Int =
    if a>b then zero
    else combine(f(a), recur(a+1))
  recur(a)

def sum(f: Int => Int) = mapReduce(f, (x, y) => x + y, 0)

sum(fact)(1, 5)

class Rational(x: Int, y: Int) {
  def numer = x

  def denom = y

  def this(x: Int) = this(x, 1)

  def add(r: Rational) =
    Rational(r.numer * denom + r.denom * numer, denom * r.denom)

  def mul(r: Rational) =
    Rational(r.numer * numer, r.denom * denom)

  def neg() =
    Rational(-numer, denom)

  def sub(r: Rational) =
  //    Rational(r.denom*numer - r.numer*denom, r.denom*denom)
    add(r.neg())

  private def gcd(a: Int, b: Int): Int =
    if b == 0 then a else gcd(b, a%b)

  override def toString() =
    s"${numer/gcd(numer, denom)}/${denom/gcd(numer, denom)}"
}


val x = Rational(1,2)
val y = Rational(3,4)
val z = Rational(4,5)
val t = Rational(5)
x.add(y).mul(z)
x.neg()
x.sub(y)

// List Class
def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case y :: ys =>
    val (more, res) = ys.span(x => x == y)
    (y :: more) :: pack(res)
}

val p1 = List(1,1,1,2,2,3)
pack(p1)

def encode[T](xs: List[T]): List[(T, Int)] =
  pack(xs).map(x => (x.head, x.size))
//  xs match {
//  case Nil => Nil
//  case y :: ys =>
//    val (more, res) = ys.span(x => x == y)
//    (y, more.size+1) :: encode(res)
//}


encode(p1)

p1 ++ List(6, 5)
p1 :: List(6, 5)
p1 ::: List(6, 5)
p1 :+ List(6,5)
p1 +: List(6,5)
p1 :: List(6, 5) :: Nil
p1 ::: List(6, 5) ::: Nil