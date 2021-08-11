abstract class IntSet:
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(s: IntSet): IntSet

  override def toString: String = super.toString

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet:
  def incl(x: Int): IntSet =
    if x < elem then NonEmpty(elem, left.incl(x), right)
    else if x > elem then NonEmpty(elem, left, right.incl(x))
    else this
  def contains(x: Int): Boolean =
    if x < elem then left.contains(x)
    else if x > elem then right.contains(x)
    else true

//  union转化为incl，拆分元素-ele/left/right，ele可以incl，继续拆left/right，
  def union(s: IntSet): IntSet =
    left.union(right).union(s).incl(elem)

  override def toString: String =
    s"${elem}${left.toString}${right.toString}"

end NonEmpty

object Empty extends IntSet:
  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = NonEmpty(x, Empty, Empty)
  def union(s: IntSet): IntSet = s
  override def toString: String =
    "end"

object IntSet:
  def apply(): IntSet = Empty
  def apply(ele: Int): IntSet = Empty.incl(ele)
  def apply(ele1: Int, ele2: Int): IntSet =
    Empty.incl(ele1).incl(ele2)


val a1 = NonEmpty(1, Empty, Empty)
val a2 = NonEmpty(2, Empty, Empty)
val a4 = NonEmpty(4, a1, a2)

val b1 = NonEmpty(3, Empty, Empty)
val b2 = NonEmpty(5, Empty, Empty)
val b4 = NonEmpty(6, b1, b2)

val c = a4.union(b4)

IntSet(1, 2)

