import scala.language.implicitConversions
import scala.language.reflectiveCalls
import scala.language.postfixOps 
import scala.util.matching.Regex

def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  (end - start)/(i * 1.0e9)
}

def scalaMatch(r: Regex, s: String) = r.findFirstIn(s).getOrElse("error")
def sulzmanMatch(r: Rexp, s: String) : String = {
    try {
        lexer(r, s)
        "succes"
    }catch {
        case _:Throwable => "error"
    }
}

def testString(n: Int) : String = "a" * n

def maxValueScala(r: Regex, n: Int) : Int = {
  try {
      scalaMatch(r, testString(n))
      maxValueScala(r, n + 100)
  }
  catch {
    case _ : Throwable => n
  }
}

def maxValue(r: Rexp, n: Int) : Int = {
    try {
      sulzmanMatch(r, testString(n))
      maxValue(r, n + 100)
  }
  catch {
    case _ : Throwable => n
  }
}

//(a?){n}a{n}
def evil1(n: Int) : Rexp = (("a"?)%(n)) ~ (("a")%(n))

def scalaEvil1(n: Int) : Regex = (s"^(a?){$n}a{$n}$$").r


/*for(i <- 1 to 5000 by 500) {
    println(i + ": " + time_needed(1, lexer(evil1(i), "a"*i + "a"*i)) + "; " + 
        time_needed(1, scalaMatch(scalaEvil1(i), "a"*i + "a"*i)))
}*/

implicit def charlist2Alt(ls: List[Char]) : Rexp = ls match {
    case head::Nil => CHAR(head)
    case head::tail => ALT(CHAR(head), charlist2Alt(tail))
}

val evil2 = ((("a" | ('b' to 'z').toList)$))%
flatten(decode(evil2, lexer(evil2, "abc")))
val scalaEvil2 = "([a-z]+;)*".r

for(i <- 0 to 5000 by 500) {
    println(i + ": " + time_needed(1, sulzmanMatch(evil2, "a"*i+"!")) + "; " + 
        time_needed(1, scalaMatch(scalaEvil2, "a"+"!")))
}

val evil3 = ("a" | "aa")%
val scalaEvil3 = "(a|aa)*".r

for(i <- 500 to 3000 by 500) {
    println(i + ": " + time_needed(1, sulzmanMatch(evil3, "a"*i+"b")) + "; " + 
        time_needed(1, scalaMatch(scalaEvil3, "a"*i+"b")))
}

val evil4 = ((("a")%)%) ~ "b"
val scalaEvil4 = "(a*)*b".r

for(i <- 500 to 5000 by 500) {
    println(i + ": " + time_needed(1, sulzmanMatch(evil4, "a"*i)) + "; " + 
        time_needed(1, scalaMatch(scalaEvil4, "a"*i)))
}

val evil5 = ("a" | ("a"?))%
val scalaEvil5 = "(a|(a?))*".r

for(i <- 500 to 3000 by 500) {
    println(i + ": " + time_needed(1, sulzmanMatch(evil5, "a"*i)) + "; " + 
        time_needed(1, scalaMatch(scalaEvil5, "a"*i)))
}