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

abstract class Rexp
case object ZERO extends Rexp
case object ONE extends Rexp
case class CHAR(c: Char) extends Rexp
case class RANGE(ls: List[Char]) extends Rexp
case class ALT(r1: Rexp, r2: Rexp) extends Rexp 
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp 
case class FROM(r: Rexp, n: Int) extends Rexp
case class BETWEEN(r: Rexp, n: Int, m: Int) extends Rexp
case class NOT(r: Rexp) extends Rexp

implicit def string2rexp(s: String) : Rexp = charlist2rexp(s.toList)

def charlist2rexp(s: List[Char]): Rexp = s match {
    case Nil => ONE
    case c::Nil => CHAR(c)
    case c::s => SEQ(CHAR(c), charlist2rexp(s))
}

def STAR(r: Rexp) : Rexp = FROM(r, 0)
def PLUS(r: Rexp) : Rexp = FROM(r, 1)
def OPTIONAL(r: Rexp) : Rexp = BETWEEN(r, 0, 1)
def NTIMES(r: Rexp, n: Int) : Rexp = BETWEEN(r, n, n)
def UPTO(r: Rexp, n: Int) : Rexp = BETWEEN(r, 0, n)
 
implicit def RegxOps(r: Rexp) = new {
    def | (s: Rexp) = ALT(r, s)
    def ~ (s: Rexp) = SEQ(r, s)
    def ? = OPTIONAL(r)
    def ! = NOT(r)
    def % = STAR(r)
    def $ = PLUS(r)
    def % (n: Int) = NTIMES(r, n)
    def < (n: Int) = UPTO(r, n)
    def > (n: Int) = FROM(r, n)
    def % (n: Int, m: Int) = BETWEEN(r, n, m)
}

implicit def stringOps(s: String) = new {
    def | (r: Rexp) = ALT(s, r)
    def | (r: String) = ALT(s, r)
    def ~ (r: Rexp) = SEQ(s, r)
    def ~ (r: String) = SEQ(s, r)
    def ? = OPTIONAL(s)
    def ! = NOT(s)
    def % = STAR(s)
    def $ = PLUS(s)
    def % (n: Int) = NTIMES(s, n)
    def < (n: Int) = UPTO(s, n)
    def > (n: Int) = FROM(s, n)
    def % (n: Int, m: Int) = BETWEEN(s, n, m)
}

def scalaMatch(r: Regex, s: String) = r.findFirstIn(s).getOrElse("error")

//(a?){n}a{n}
def evil1(n: Int) : Rexp = (("a"?)%(n)) ~ (("a")%(n))

def scalaEvil1(n: Int) : Regex = (s"^(a?){$n}a{$n}$$").r


for(i <- 1 to 5000 by 500) {
    println(i + ": " + time_needed(1, lexer(evil1(i), "a"*i + "a"*i)) + "; " + 
        time_needed(1, scalaMatch(scalaEvil1(i), "a"*i + "a"*i)))
}

val evil2 = ((PLUS(ALT(("a" | "e"), ("i" | "o"))))%) ~ "b"

