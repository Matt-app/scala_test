package recfun


object DynamicProgramming {
  
//  1、数字三角形
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

//  动态规划
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
  
//  2、最长公共子串，输出两个字符串顺序不变的情况下最长的相同子字符串。
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
//     if sa1(i-1) == sa2(j-1) then mCom(i)(j) = math.max(mCom(i-1)(j), mCom(i)(j-1)) + 1 
//     按理说，如果发现两个字母相同，两个集合都去掉相同的字母，然后index两边同时加一；我写的是，如果发现这两个字母相同，其中一个集合去掉当前字母并且index加一，另一个不变。
        else mCom(i)(j) = math.max(mCom(i-1)(j), mCom(i)(j-1))
    mCom(l1)(l2)
  }
  
//  3、help jimmy
//  没有现成的题目，太难了=，=
  def helpJimmy(t:Array[Array[Int]]):Int ={
//  先排序
//  下面有没有板
    val jimmy = t.head
    val maxHigh = jimmy(3)
    val numPlatform = jimmy(0)
    val platforms = t.tail.sortBy(-_(2))
    var leftMinTime = Array[Int](numPlatform)
    var rightMinTime = Array[Int](numPlatform)
    1
  }
  
//  4.最佳加法表达式，子问题是前l-1个数字添加m-1个加号的最小值。未做空间优化
  def bestAdd(s:String, m:Int) ={
    val l = s.length
    var minSum = Array.ofDim[Int](m+1, l)
    for (i <- 0 to l-1) {
      minSum(0)(i) = s.substring(0, i+1).toInt
    }
    if m > 1 then
      for (i <- 1 to m)
        for (j <- 0 to i-1)
          minSum(i)(j) = Int.MaxValue - s.toInt

    for (i <- 1 to m)
      for (j <- i to l-1)
        var tmpSum = List.empty[Int]
        for (k <- 0 to j-1)
        tmpSum = tmpSum.appended(minSum(i-1)(k) + s.substring(k+1, j+1).toInt)
        minSum(i)(j) = tmpSum.min
    minSum(m)(l-1)
  }

//滑雪问题，脑子：我懂了，先看别的，这个留着下次写
  def skiing() = {
    ???
  }

  def main(args: Array[String]): Unit = {
//    println(maxSum_2(0,0))
//    println(maxSum_4())
//    println(maxCommonStr("ilovechina", "ilikechinesepeople"))
//     println(helpJimmy(Array[Array[Int]](Array(3,8,17,20),Array(0,10,8),Array(0,10,13),Array(4,14,3))))
    println(bestAdd("54321", 2))
  }
}
