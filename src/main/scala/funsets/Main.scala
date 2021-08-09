package funsets

object Main extends App:
  import FunSets.*
  val a = Set(11,22,33)
  val b = List(1,2,3)
  b(1)
  println(b(2))
  println(singletonSet(1))
  println(contains(singletonSet(1), 1))
  println(contains(Set(21,22,23,24,5), 3))

