abstract class Nat:
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat
  def + (that: Nat): Nat
  def - (that: Nat): Nat
  def < (that: Nat): Boolean
  def > (that: Nat): Boolean
  def * (that: Nat): Nat
  override def toString: String = super.toString

class Succ(n: Nat) extends Nat:
  override def isZero: Boolean = false
  override def successor: Nat = Succ(this)
  override def +(that: Nat): Nat = Succ(n + that)
  override def -(that: Nat): Nat = if that.isZero then this else n - that.predecessor
  override def predecessor: Nat = n
  override def toString: String = s"Succ($n)"
  override def *(that: Nat): Nat =
    if that.isZero then that
    else mul(that.predecessor, this, this)
  def mul(a: Nat, b: Nat, c: Nat): Nat =
    if a.isZero then b
    else mul(a.predecessor, b+c, c)
  override def >(that: Nat): Boolean = if that.isZero then true else n > that.predecessor
  override def <(that: Nat): Boolean = if that.isZero then false else n < that.predecessor
end Succ

object Zero extends Nat:
  override def isZero: Boolean = true
  override def successor: Nat = Succ(this)
  override def +(that: Nat): Nat = that
  override def -(that: Nat): Nat = if that.isZero then that else ???
  override def predecessor: Nat = ???
  override def toString: String = "Zero"
  override def *(that: Nat): Nat = this
  override def >(that: Nat): Boolean = false
  override def <(that: Nat): Boolean = if that.isZero then false else true

val two = Succ(Succ(Zero))
val one = Succ(Zero)
val three = two + one
two - one
one - two
three > two
two > three
two * three
three * three