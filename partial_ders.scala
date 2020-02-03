import scala.language.implicitConversions
import scala.language.reflectiveCalls
import scala.language.postfixOps 

abstract class Rexp
case object ZERO extends Rexp
case object ONE extends Rexp
case class CHAR(c: Char) extends Rexp
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp
case class ALT(r1: Rexp, r2: Rexp) extends Rexp
case class STAR(r: Rexp) extends Rexp

def nullable(r: Rexp) : Boolean = r match {
    case ZERO => false
    case ONE => true
    case CHAR(_) => false
    case SEQ(r1, r2) => nullable(r1) && nullable(r2)
    case ALT(r1, r2) => nullable(r1) || nullable(r2)
    case STAR(_) => true
}


def charlist2rexp(s: List[Char]): Rexp = s match {
    case Nil => ONE
    case c::Nil => CHAR(c)
    case c::s => SEQ(CHAR(c), charlist2rexp(s))
}

implicit def string2rexp(s: String) : Rexp = charlist2rexp(s.toList)

implicit def RegxOps(r: Rexp) = new {
    def | (s: Rexp) = ALT(r, s)
    def ~ (s: Rexp) = SEQ(r, s)
    def % = STAR(r)
}

implicit def stringOps(s: String) = new {
    def % = STAR(s)
    def | (r: Rexp) = ALT(s, r)
    def | (r: String) = ALT(s, r)
    def ~ (r: Rexp) = SEQ(s, r)
    def ~ (r: String) = SEQ(s, r)
}


def pder(c: Char, r: Rexp): Set[Rexp] = r match {
    case ZERO => Set()
    case ONE => Set()
    case CHAR(d) => if (c == d) Set(ONE) else Set()
    case ALT(r1, r2) => pder(c, r1) ++ pder(c, r2)
    case SEQ(r1, r2) =>
        (for (pr1 <- pder(c, r1)) yield SEQ(pr1, r2)) ++
        (if (nullable(r1)) pder(c, r2) else Set())
    case STAR(r1) =>
        for (pr1 <- pder(c, r1)) yield SEQ(pr1, STAR(r1))
}

def pders(cs: List[Char], r: Rexp): Set[Rexp] = cs match {
    case Nil => Set(r)
    case c::cs => pder(c, r).flatMap(pders(cs, _))
}

val reg1 = "a"
val reg2 = "a" ~ "b"
val reg3 = "a" | "b"
val reg4 = "a"%
val reg5 = ("a" ~ "b")%
val reg6 = (("a" | "c") ~ "d")%
