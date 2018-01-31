import scala.io.Source


val in = Source.fromURL("https://lamp.epfl.ch/files/content/" +
  "sites/lamp/files/teaching/progfun/linuxwords.txt")

val words = in.getLines.toList filter (word => word.forall(w => w.isLetter))

/* define the map of numbers to letters */
val nmem = Map('2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL", '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")

val charCode = for ((digit, str) <- nmem; char <- str) yield char -> digit

// Java = 5282
def wordCode(word: String): String = word.toUpperCase map charCode

wordCode("Java")

// "5282" = "JAVA"
val wordsForNum: Map[String, Seq[String]] = words groupBy wordCode withDefaultValue Seq()

def encode(number: String): Set[List[String]] =
  if(number.isEmpty) Set(List())
  else {
    for {
      split <- 1 to number.length
      word <- wordsForNum(number take split)
      rest <- encode(number drop split)
    } yield word :: rest
  }.toSet


encode("7225547386")



def translate(number: String): Set[String] =
  encode(number) map( sentence => sentence.mkString(" "))

translate("7225247386") foreach println