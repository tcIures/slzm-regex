import scala.language.implicitConversions
import scala.language.reflectiveCalls
import scala.annotation.tailrec 
import scala.language.postfixOps  

//Bitcoded version of Sulzman and Lu's algorithm
abstract class Rexp
case object ZERO extends Rexp
case object ONE extends Rexp
case class CHAR(c: Char) extends Rexp
case class ALT(r1: Rexp, r2: Rexp) extends Rexp
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp
case class STAR(r: Rexp) extends Rexp

abstract class Val
case object Empty extends Val
case class Chr(c: Char) extends Val
case class Sequ(c1: Val, v2: Val) extends Val
case class Left(v: Val) extends Val
case class Right(v: Val) extends Val
case class Stars(vs: List[Val]) extends Val

abstract class ARexp
case object AZERO extends ARexp
case class AONE(bs: List[Boolean]) extends ARexp
case class ACHAR(bs: List[Boolean], c: Char) extends ARexp
case class AALT(bs: List[Boolean], r1: ARexp, r2: ARexp) extends ARexp
case class ASEQ(bs: List[Boolean], r1: ARexp, r2: ARexp) extends ARexp
case class ASTAR(bs: List[Boolean], r: ARexp) extends ARexp

def decode_aux(r: Rexp, bs: List[Boolean]) : (Val, List[Boolean]) = (r, bs) match {
    case (ONE, bs) => (Empty, bs)
    case (CHAR(c), bs) => (Chr(c), bs)
    case (ALT(r1, r2), false::bs) => {
        val (v, bs1) = decode_aux(r1, bs)
        (Left(v), bs1)
    }
    case (ALT(r1, r2), true::bs) => {
        val (v, bs1) = decode_aux(r2, bs)
        (Right(v), bs1)
    }
    case (SEQ(r1, r2), bs) => {
        val (v1, bs1) = decode_aux(r1, bs)
        val (v2, bs2) = decode_aux(r2, bs1)
        (Sequ(v1, v2), bs2)
    }
    case (STAR(r), false::bs) => {
        val (v, bs1) = decode_aux(r, bs)
        val (Stars(vs), bs2) = decode_aux(STAR(r), bs1)
        (Stars(v::vs), bs2)
    }
    case (STAR(_), true::bs) => (Stars(Nil), bs)
}

def decode(r: Rexp, bs: List[Boolean]) = decode_aux(r, bs) match {
    case (v, Nil) => v
    case _ => throw new Exception("Not decodable")
}

def bnullable(r: ARexp) : Boolean = r match {
    case AZERO => false
    case AONE(_) => true
    case ACHAR(_,_) => false
    case AALT(_, r1, r2) => bnullable(r1) || bnullable(r2)
    case ASEQ(_, r1, r2) => bnullable(r1) && bnullable(r2)
    case ASTAR(_,_) => true
}

def fuse(bs: List[Boolean], r: ARexp) : ARexp = r match {
    case AZERO => AZERO
    case AONE(bsq) => AONE(bs ++ bsq)
    case ACHAR(bsq, c) => ACHAR(bs ++ bsq, c)
    case AALT(bsq, r1, r2) => AALT(bs ++ bsq, r1, r2)
    case ASEQ(bsq, r1, r2) => ASEQ(bs ++ bsq, r1, r2)
    case ASTAR(bsq, r) => ASTAR(bs ++ bsq, r)
}

def bmkeps(r: ARexp) : List[Boolean] = r match {
    case AONE(bs) => bs
    case AALT(bs, r1, r2) => if(bnullable(r1)) bs ++ bmkeps(r1)
                                else bs ++ bmkeps(r2)
    case ASEQ(bs, r1, r2) => bs ++ bmkeps(r1) ++ bmkeps(r2)
    case ASTAR(bs, r) => bs ++ List(true)
}

def bder(r: ARexp, c: Char) : ARexp = r match {
    case AZERO => AZERO
    case AONE(_) => AZERO
    case ACHAR(bs, d) => if(d == c) AONE(bs) else AZERO
    case AALT(bs, r1, r2) => AALT(bs, bder(r1, c), bder(r2, c))
    case ASEQ(bs, r1, r2) => if(bnullable(r1)) 
                                AALT(bs, ASEQ(List[Boolean](), bder(r1, c), r2), fuse(bmkeps(r1), bder(r2, c)))
                            else ASEQ(bs, bder(r1, c), r2)
    case ASTAR(bs, r) => ASEQ(bs, fuse(List(false), bder(r, c)), ASTAR(List[Boolean](), r))
}

def internalise(r: Rexp) : ARexp = r match {
    case ZERO => AZERO
    case ONE => AONE(List[Boolean]())
    case CHAR(c) => ACHAR(List[Boolean](), c)
    case ALT(r1, r2) => AALT(List[Boolean](), fuse(List(false), internalise(r1)), 
                                fuse(List(true), internalise(r2)))
    case SEQ(r1, r2) => ASEQ(List[Boolean](), internalise(r1), internalise(r2))
    case STAR(r) => ASTAR(List[Boolean](), internalise(r))
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

@tailrec
def bders(r: ARexp, s: List[Char]) : ARexp = s match {
    case Nil => r
    case c::cs => bders(bder(r, c), cs)
}

def blex(r: ARexp, s: List[Char]) : List[Boolean] = s match {
    case Nil => if(bnullable(r)) bmkeps(r) else throw new Exception("Not matched")
    case c::cs => blex(bder(r, c), cs)
}

def blexer(r: Rexp, s: String) : List[Boolean] = blex(internalise(r), s.toList)

def blex_info(r: ARexp, s: List[Char]) : (List[Boolean], Int) = s match {
    case Nil => if(bnullable(r)) (bmkeps(r), size(r)) else throw new Exception("Not matched")
    case c::cs => blex_info(bder(r, c), cs)
}

def blexer_info(r: Rexp, s: String) : (List[Boolean], Int) = blex_info(internalise(r), s.toList)

def flatten(v: Val) : String = v match {
    case Empty => ""
    case Chr(c) => c.toString
    case Left(v) => flatten(v)
    case Right(v) => flatten(v)
    case Sequ(v1, v2) => flatten(v1) + flatten(v2)
    case Stars(vs) => vs.map(flatten).mkString
}

def size(r: ARexp) : Int = r match {
    case AZERO => 1
    case AONE(_) => 1
    case ACHAR(_, _) => 1
    case AALT(_, r1, r2) => 1 + size(r1) + size(r2)
    case ASEQ(_, r1, r2) => 1 + size(r1) + size(r2)
    case ASTAR(_, r) => 1 + size(r)
}

val reg0 = ("a" | "ab") ~ (("ab" | "")%)
val reg1 = ("a" | "b")
val reg2 = STAR("ab" | "c")
val reg3 = STAR("abc" | "d")

def stringDecode(r: Rexp, ls: List[Boolean]): String = flatten(decode(r, ls))

val EVIL2 = SEQ(STAR(STAR(CHAR('a'))), CHAR('b'))

val evil0 = ((("a")%)%) ~ "b"


// for measuring time
def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  (end - start) / (i * 1.0e9)
}

val str = "a"*5 + "b"

blexer_info(evil0, str)._2

val reg0 = (("a"|"b")~"c")%
val str = "acbc"*100
blexer_info(reg0, str)._2

val evil0 = ((("a")%)%) ~ "b"
val str = "a"*10 + "b"
blexer_info(evil0, str)._2

val reg0 = ((("a"|"b")~"c")%)%
val str = "acbc"*1
blexer_info(reg0, str)._2

time_needed(1, blexer(reg0, "acbc" * 30))