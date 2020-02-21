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

implicit def charlist2Alt(ls: List[Char]) : Rexp = ls match {
    case head::Nil => CHAR(head)
    case head::tail => ALT(CHAR(head), charlist2Alt(tail))
}


