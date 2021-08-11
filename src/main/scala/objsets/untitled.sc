trait LIST[T]:
  def isEmpty: Boolean
  def head: T
  def tail: LIST[T]

class CONS[T](val head: T, val tail: LIST[T]) extends LIST[T]:
  override def isEmpty: Boolean = false

class NIL[T] extends LIST[T]:
  override def isEmpty: Boolean = true
  override def head: T = throw new NoSuchElementException("Nil.head")
  override def tail: LIST[T] = throw new NoSuchElementException("Nil.tail")

def nth[T](xs: LIST[T], n:Int): T =
  if xs.isEmpty then throw IndexOutOfBoundsException()
  else if n == 0 then xs.head
  else nth(xs.tail, n-1)


val a1 = CONS(1, CONS(2, NIL()))
println(nth(a1, 0))

val a = "apple and me"
a.contains("apple")
a.contains(List("apple", "and", "me"))
a.contains(List(" ", "apple", "and", "me"))

val b = List(" ", "apple", "and", "me")
b.exists(x => a.contains(x))

