import forcomp.Anagrams.{Occurrences, Sentence, combinations, dictionaryByOccurrences, sentenceOccurrences, subtract, wordOccurrences}
import forcomp.Coder


//Vctor -> 32 * 32 most 5-level
val v = Vector(1,2,3,4)
v :+ 5
5 +: v
v.filter(_ > 3)
(1 to 9).map(x => (1 to 6).map(( x,_)))

def isPrime(n: Int): Boolean =
  (2 until n).forall(n%_!=0)

isPrime(7)
isPrime(6)
isPrime(31)

for
  i <- 1 until 9
  j <- 1 until i
  if isPrime(i+j)
yield(i, j)

def isSafe(queens: List[Int], col: Int): Boolean =
  def isSafeDelta(queens: List[Int], col: Int, delta: Int): Boolean = queens match {
    case Nil => true
    case queen :: others =>
      if queen == col || (col-queen).abs == delta then false
      else isSafeDelta(others, col, delta+1)
  }
  isSafeDelta(queens, col, 1)

def queens(n: Int) =
  def placeQueen(int: Int): Set[List[Int]] =
    if int == 0 then Set(List())
    else for
      queens <- placeQueen(int - 1)
      col <- 0 until n
      if isSafe(queens, col)
    yield col :: queens
  placeQueen(n)

queens(4)

val c = new Coder(List("Scala", "Python", "Ruby", "C"))
c.encode("122")
c.encode("72252")

val word = "newYorkNewMe"
val word1 = "oldMeOldUs"
val sentence = List(word, word1)

def combines(occurrences: Occurrences) =
  def placeQueen(occur: Occurrences): List[Occurrences] = occur match {
    case Nil => List(List())
    case (x, y) :: ys =>
      for queens <- placeQueen(ys);num <- 0 to y yield (x, num) :: queens
  }
  placeQueen(occurrences).map(for i <- _ if i._2>0 yield i)

combines(wordOccurrences(word1))

val wo1 = wordOccurrences(word)

combines(List(('a', 2), ('b', 2)))


???
val lard = List(('a', 1), ('y', 1), ('l', 1), ('r', 1))
val r = List(('r', 1))
val lad = List(('a', 1), ('d', 1), ('l', 1))

subtract(lard, r)

subtract(lard, r)
lard.groupBy(_._1).map((x, y) => y(0))
Map(1->2).withDefaultValue(0)

subtract(lard, r)
lard.sorted

def sentenceAnagrams(sentence: Sentence): List[Sentence] =
  def placeQueen(occur: Occurrences): List[Sentence] =
    for com <- combinations(occur); pq <- placeQueen(subtract(com, occur))
      if !dictionaryByOccurrences(com).isEmpty && subtract(com, occur).isEmpty
        dbo <- dictionaryByOccurrences(com)
    yield dbo :: pq

  if sentence.isEmpty then List(Nil)
  else placeQueen(sentenceOccurrences(sentence))

sentenceAnagrams(List())
sentenceOccurrences(List())
sentenceOccurrences(List())

