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
        decode(r, lexer(r, s))
        "succes"
    }catch {
        case _:Throwable => "error"
    }
}

implicit def charlist2Alt(ls: List[Char]) : Rexp = ls match {
    case head::Nil => CHAR(head)
    case head::tail => ALT(CHAR(head), charlist2Alt(tail))
}

val evil3 = ("a" | "aa")%
val scalaEvil3 = "(a|aa)*".r

for(i <- 500 to 3000 by 500) {
    println(i + ": " + time_needed(1, sulzmanMatch(evil3, "a"*i+"b")))
}

val evil2 = ((("a" | ('b' to 'z').toList)%))%
val scalaEvil2 = "([a-z]*)*".r

for(i <- 0 to 10000 by 1000) {
    println(i + ": " + time_needed(1, sulzmanMatch(evil2, "a"*i+"!")) + "; " + 
        time_needed(1, scalaMatch(scalaEvil2, "a"+"!")))
}


