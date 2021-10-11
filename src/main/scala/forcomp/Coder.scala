package forcomp

class Coder (words: List[String]):
  val mnemonics = Map('2' -> "abc", '3'-> "def", '4' -> "ghi", '5'-> "jkl", '6'->"mno", '7'->"pqrs", '8'->"tuv", '9'->"wxyz")

  private val charCode: Map[Char, Char] =
    for (digit, letters) <- mnemonics;l <- letters yield l -> digit

  private def wordCode(word: String): String =
    word.toLowerCase.map(charCode)

  private val wordsForNum: Map[String, List[String]] =
    words.groupBy(wordCode).withDefaultValue(Nil)

  def encode(num: String): Set[List[String]] =
    if num.isEmpty then Set(Nil)
    else
      for
        splitPoint <- (1 to num.length).toSet
        word <- wordsForNum(num.take(splitPoint))
        rest <- encode(num.drop(splitPoint))
      yield word :: rest
      
      
