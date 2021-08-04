package recfun

object test {
  
//  三角形初始化
  val D = Array[Array[Int]](Array(7),Array(3,8),Array(8,1,0),Array(2,7,4,4),Array(4,5,2,6,5))
  val N = 4
  val mSum = Array.ofDim[Int](N+1,N+1)
  for (i <- 0 to N)
    for (j <- 0 to i)
      mSum(i)(j) = -1
  
//  递归：2的n次方
  def maxSum_1(r: Int, l: Int):Int = {
    if r == N then D(r)(l) else D(r)(l)+math.max(maxSum_1(r+1, l), maxSum_1(r+1, l+1))
  }

//  保存变量的递归：n的2次方
  def maxSum_2(r: Int, l: Int):Int = {  
    if mSum(r)(l) != -1 then mSum(r)(l)
    if r == N then mSum(r)(l) = D(r)(l)
    else mSum(r)(l) = D(r)(l) + math.max(maxSum_2(r + 1, l), maxSum_2(r + 1, l + 1))
    mSum(r)(l)
  }

//  正则
  def maxSum_3():Int = {
    val mSum = Array.ofDim[Int](N+1,N+1)
    for (i <- 0 to N) mSum(N)(i) = D(N)(i)
    for (i <- (0 to N-1).reverse)
      for (j <- 0 to i)
        mSum(i)(j) = D(i)(j) + math.max(mSum(i+1)(j), mSum(i+1)(j+1))
    mSum(0)(0)
  }

  //  正则空间优化，感觉没有必要这么极限，这样有可能会更改D吧，谁知道呢，还不如重新创建一个一维数组，C++这里相当于一个地址指针。
  def maxSum_4():Int = {
    val mSum = D(N)
    for (i <- (0 to N-1).reverse)
      for (j <- 0 to i)
        mSum(j) = D(i)(j) + math.max(mSum(j), mSum(j+1))
    mSum(0)
  }
  
//  最长公共子串
  def maxCommonStr(s1:String, s2:String):Int = {
    val l1 = s1.length
    val l2 = s2.length
    val sa1 = s1.toCharArray
    val sa2 = s2.toCharArray
    val l = math.max(l1, l2)
    val mCom = Array.ofDim[Int](l+1,l+1)
    for (i <- 0 to l) {
      mCom(i)(0) = 0
      mCom(0)(i) = 0
    }
    for (i <- 1 to l1)
      for (j <- 1 to l2)
        if sa1(i-1) == sa2(j-1) then mCom(i)(j) = mCom(i-1)(j-1) + 1
//          mistake
//      if sa1(i-1) == sa2(j-1) then mCom(i)(j) = math.max(mCom(i-1)(j), mCom(i)(j-1)) + 1 
        else mCom(i)(j) = math.max(mCom(i-1)(j), mCom(i)(j-1))
    mCom(l1)(l2)
  }
  
//  help jimmy
  def helpJimmy(t:Array[Array[Int]]):Int ={
    val jimmy = t.head
    val maxHigh = jimmy(3)
    val numPlatform = jimmy(0)
    val minDistance = Array.ofDim[Int](numPlatform,numPlatform)
    minDistance(0) = Array(jimmy(2)+math.abs(t(1)(0)-jimmy(1)),jimmy(2)+math.abs(t(1)(1)-jimmy(1)))
    

    1
  }
  
  def main(args: Array[String]): Unit = {
//    println(maxSum_2(0,0))
//    println(maxSum_4())
    println(maxCommonStr("abcdef", "aaaaef"))
    println(helpJimmy(Array[Array[Int]](Array(2, 6, 6, 5),Array(2, 6, 10, 5),Array(3,8,6),Array(1,6,4))))
  }
}
