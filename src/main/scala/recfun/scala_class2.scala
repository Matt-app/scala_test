package recfun

import math.max
import math._

object scala_class2 {
//  higher-order functions 接收functions为参数
  def sumInts(f: Int => Int, a: Int, b: Int): Int =
    if a > b then 0 else f(a) + sumInts(f, a + 1, b)
//  Take the sum of the cubes of all the integers between a and b :
  def cube(x: Int): Int = x * x * x

  def sumCubes(a: Int, b: Int): Int =
    if a > b then 0 else cube(a) + sumCubes(a + 1, b)

  def fact(x: Int): Int = if x == 0 then 1 else x * fact(x - 1)

  def sumFactorials(a: Int, b: Int): Int =
    if a > b then 0 else fact(a) + sumFactorials(a + 1, b)

  def sum(f: Int => Int, a: Int, b: Int): Int =
    def loop(a: Int, acc: Int): Int =
      if a > b then acc
      else loop(a+1, acc+f(a))
    loop(a, 0)


//    currying
  def sum(f: Int => Int): (Int, Int) => Int =
    def sumF(a: Int, b: Int): Int =
        if a>b then 0
        else f(a) + sumF(a + 1, b)
    sumF

//    优化
  def sum_1(f: Int => Int)(a: Int, b: Int): Int =
    if a > b then 0 else f(a) + sum_1(f)(a+1, b)
//    In fact, this idea that you can write every function, is essentially a sequence of anonymous functions that each
  //    take a single parameter. It goes back even further to Schonfinkel and Frege. But the term currying has stuck
  
  def main(args: Array[String]): Unit = {
    println(sumCubes(1,2))
    println(sumInts(cube, 1, 2))
    println(sumInts(x => x*x*x, 1, 2))
    println(sum(x=>x*x*x, 1, 2))
    println(sum(x => x*x*x)(1, 2))
  }

}
