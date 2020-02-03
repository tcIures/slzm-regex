import scala.language.implicitConversions
import scala.language.reflectiveCalls
import scala.annotation.tailrec 
import scala.language.postfixOps  
import collection.immutable.ListMap
import scala.collection._
import mutable.ListBuffer

//Bitcoded version of Sulzman and Lu's algorithm
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

abstract class ARexp
case object AZERO extends ARexp
case class AONE(bs: List[Boolean]) extends ARexp
case class ACHAR(bs: List[Boolean], c: Char) extends ARexp
case class ARANGE(bs: List[Boolean], ls: List[Char]) extends ARexp
case class AALT(bs: List[Boolean], l: List[ARexp]) extends ARexp
case class ASEQ(bs: List[Boolean], r1: ARexp, r2: ARexp) extends ARexp
case class AFROM(bs: List[Boolean], r: ARexp, n: Int) extends ARexp
case class ABETWEEN(bs: List[Boolean], r: ARexp, n: Int, m: Int) extends ARexp
case class ANOT(bs: List[Boolean], r: ARexp) extends ARexp

abstract class Val
case object Empty extends Val
case class Chr(c: Char) extends Val
case class Sequ(c1: Val, v2: Val) extends Val
case class Left(v: Val) extends Val
case class Right(v: Val) extends Val
case class From(vs: List[Val]) extends Val
case class Between(vs: List[Val]) extends Val
case class Not(v: Val) extends Val

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
    case (FROM(r,_), false::bs) => {
        val (v, bs1) = decode_aux(r, bs)
        val (From(vs), bs2) = decode_aux(FROM(r,0), bs1)
        (From(v::vs), bs2)
    }
    case (FROM(_,_), true::bs) => (From(Nil), bs)
    case (BETWEEN(r,_,_), false::bs) => {
        val (v, bs1) = decode_aux(r, bs)
        val (Between(vs), bs2) = decode_aux(BETWEEN(r,0,0), bs1)
        (Between(v::vs), bs2)
    }
    case (BETWEEN(_,_,_), true::bs) => (Between(Nil), bs)
    case (NOT(r), bs) => {
        val (v, bs1) = decode_aux(r, bs)
        (Not(v), bs1)
    }
}

def decode(r: Rexp, bs: List[Boolean]) = decode_aux(r, bs) match {
    case (v, Nil) => v
    case _ => throw new Exception("Not decodable")
}

def nullable(r: ARexp) : Boolean = r match {
    case AZERO => false
    case AONE(_) => true
    case ACHAR(_,_) => false
    case ARANGE(_,_) => false
    case AALT(bs, ls) => ls match {
        case Nil => false
        case head::tail => nullable(head) || nullable(AALT(bs, tail))
    }
    case ASEQ(_, r1, r2) => nullable(r1) && nullable(r2)
    case AFROM(_,r, i) => if(i == 0) true else nullable(r)
    case ABETWEEN(_,r, i, _) => if(i == 0) true else nullable(r)
    case ANOT(_,r) => !nullable(r)
}

def fuse_reversed(bs: List[Boolean], r: ARexp) : ARexp = r match {
    case AZERO => AZERO
    case AONE(bsq) => AONE(bsq ++ bs)
    case ACHAR(bsq, c) => ACHAR(bsq ++ bs, c)
    case AALT(bsq, ls) => AALT(bsq ++ bs, ls)
    case ASEQ(bsq, r1, r2) => ASEQ(bsq ++ bs, r1, r2)
    case AFROM(bsq, r, n) => AFROM(bsq++bs, r, n)
    case ABETWEEN(bsq, r, n, m) => ABETWEEN(bsq++bs, r, n, m)
    case ANOT(bsq, r) => ANOT(bsq++bs, r)
}

def fuse(bs: List[Boolean], r: ARexp) : ARexp = r match {
    case AZERO => AZERO
    case AONE(bsq) => AONE(bs ++ bsq)
    case ACHAR(bsq, c) => ACHAR(bs ++ bsq, c)
    case AALT(bsq, ls) => AALT(bs ++ bsq, ls)
    case ASEQ(bsq, r1, r2) => ASEQ(bs ++ bsq, r1, r2)
    case AFROM(bsq, r, n) => AFROM(bs++bsq, r, n)
    case ABETWEEN(bsq, r, n, m) => ABETWEEN(bs++bsq, r, n, m)
    case ANOT(bsq, r) => ANOT(bs++bsq, r)
}

def delete(r: ARexp): Rexp = r match{
    case AZERO => ZERO
    case AONE(_) => ONE
    case ACHAR(_, c) => CHAR(c)
    case AALT(bs, Nil) => ZERO
    case AALT(bs, head::Nil) => delete(head)
    case AALT(bs, head1::head2::tail) => ALT(delete(head1), delete(head2))
    case AALT(bs, head::tail) => ALT(delete(head), delete(AALT(bs, tail)))
    case ASEQ(_, r1, r2) => SEQ(delete(r1), delete(r2))
    case AFROM(bsq, r, n) => FROM(delete(r), n)
    case ABETWEEN(bsq, r, n, m) => BETWEEN(delete(r), n, m)
    case ANOT(bsq, r) => NOT(delete(r))
}

def mkeps(r: ARexp) : List[Boolean] = r match {
    case AONE(bs) => bs
    case AALT(bs, ls) => val matched = ls.find(x => nullable(x)).getOrElse(AZERO); bs ++ mkeps(matched)
    case ASEQ(bs, r1, r2) => bs ++ mkeps(r1) ++ mkeps(r2)
    case AFROM(bs, r, n) => bs ++ List(true)
    case ABETWEEN(bs, r, n, m) => bs ++ List(true)
    case ANOT(bs, r) => bs ++ mkeps(r)
}

def der(r: ARexp, c: Char) : ARexp = r match {
    case AZERO => AZERO
    case AONE(_) => AZERO
    case ACHAR(bs, d) => if(d == c) AONE(bs) else AZERO
    case AALT(bs, ls) => AALT(bs, ls.map(x => der(x, c)))
    case ASEQ(bs, r1, r2) => if(nullable(r1)) 
                                 AALT(bs, List(ASEQ(List(), der(r1, c), r2), fuse(mkeps(r1), der(r2, c))))
                            else ASEQ(bs, der(r1, c), r2)
    case AFROM(bs, r, i) =>
        if(i == 0) ASEQ(bs, fuse(List(false), der(r, c)), AFROM(List(), r, 0)) 
        else ASEQ(bs, fuse(List(false), der(r, c)), AFROM(List(), r, i-1))
    case ABETWEEN(bs, r, i, j) => (i, j) match {
        case(0, 0) => AZERO
        case(0, i) => ASEQ(bs, fuse(List(false), der(r, c)), ABETWEEN(List(), r, 0, i - 1))
        case(i, j) => ASEQ(bs, fuse(List(false), der(r, c)), ABETWEEN(List(), r, i - 1, j - 1))
    }
    case ANOT(bs, r) => ANOT(bs, der(r, c))
}

def rmvZeroDup(ls: List[ARexp], set: Set[Rexp]): List[ARexp] = ls match{
    case Nil => List()
    case head::Nil =>  head match {
        case AZERO => List()
        case _ => if(!(set contains delete(head))) List(head) 
                    else List()
    }
    case head::tail => head match {
        case AZERO => rmvZeroDup(tail, set)
        case _ => if(!(set contains delete(head)))
            List(head) ++ rmvZeroDup(tail, set ++ Set(delete(head)))
            else rmvZeroDup(tail, set)
    }
}

def getLs(r: ARexp): List[ARexp] = r match {
    case AALT(bs, ls) => flatten_alt(ls.map(e => fuse(bs, e)))
    case r => List(r)
}

def flatten_alt(ls: List[ARexp]) : List[ARexp] = ls match {
    case Nil => List()
    case head::Nil => getLs(head)
    case head::tail => getLs(head) ++ flatten_alt(tail)
}

def emptyBsq(r: ARexp) : ARexp = r match {
    case AZERO => AZERO
    case AONE(_) => AONE(List())
    case ACHAR(_, c) => ACHAR(List(), c)
    case AALT(_, ls) => AALT(List(), ls)
    case ASEQ(_, r1, r2) => ASEQ(List(), r1, r2)
    case AFROM(bsq, r, n) => AFROM(List(), r, n)
    case ABETWEEN(bsq, r, n, m) => ABETWEEN(List(), r, n, m)
    case ANOT(bsq, r) => ANOT(List(), r)
}

def getBsq(r: ARexp) = r match{
    case AZERO => List()
    case AONE(bsq) => bsq
    case ACHAR(bsq, _) => bsq
    case AALT(bsq, _) => bsq
    case ASEQ(bsq, _, _) => bsq
    case AFROM(bsq, r, n) => bsq
    case ABETWEEN(bsq, r, n, m) => bsq
    case ANOT(bsq, r) => bsq
}

def multiplyRight(ls: List[ARexp], r: ARexp) : List[ARexp] = {
    for(r1 <- ls) yield ASEQ(getBsq(r1), emptyBsq(r1), r)
}

def multiplyLeft(r: ARexp, ls: List[ARexp]) : List[ARexp] = {
   for(r1 <- ls) yield ASEQ(getBsq(r1), r, emptyBsq(r1))
}

def multiply(ls1: List[ARexp], ls2: List[ARexp]) = {
    for(r1 <- ls1; r2 <- ls2) yield ASEQ(getBsq(r1)++getBsq(r2), emptyBsq(r1), emptyBsq(r2))
}

def simp(r: ARexp) : ARexp = r match {
    case ASEQ(bs, r1, r2) =>  (simp(r1), simp(r2)) match {
        case (AZERO, _) => AZERO
        case (_, AZERO) => AZERO
        case (AONE(bs1), r2s) => fuse(bs++bs1, r2s)
        case (r1s, AONE(bs1)) => fuse(bs, fuse_reversed(bs1, r1s))
        case (AALT(bs1, ls1), AALT(bs2, ls2)) => AALT(bs++bs1++bs2, multiply(ls1, ls2))
        case (AALT(bs1, ls), r2s) => AALT(bs++bs1, multiplyRight(ls, r2s))
        case (r1s, AALT(bs1, ls)) => AALT(bs++bs1, multiplyLeft(r1s, ls))
        case (r1s, r2s) => ASEQ(bs, r1s, r2s)
    }
    case AALT(bs, ls) => ls match {
        case Nil => AZERO
        case head::Nil => fuse(bs, head)
        case head::tail => {
            val newL = rmvZeroDup(flatten_alt(List(head)++tail), Set()).map(simp(_))
            AALT(bs, newL)
        }
    }
    case AFROM(bs, r, n) => AFROM(bs, simp(r), n)
    case ABETWEEN(bs, r, n, m) => ABETWEEN(bs, simp(r), n, m)
    case r => r
}

def internalise(r: Rexp) : ARexp = r match {
    case ZERO => AZERO
    case ONE => AONE(List())
    case CHAR(c) => ACHAR(List(), c)
    case ALT(r1, r2) => AALT(List(), List(fuse(List(false), internalise(r1)), 
                                                    fuse(List(true), internalise(r2))))
    case SEQ(r1, r2) => ASEQ(List(), internalise(r1), internalise(r2))
    case FROM(r, n) => AFROM(List(), internalise(r), n)
    case BETWEEN(r, n, m) => ABETWEEN(List(), internalise(r), n, m)
    case NOT(r) => ANOT(List(), internalise(r))
}

def charlist2rexp(s: List[Char]): Rexp = s match {
    case Nil => ONE
    case c::Nil => CHAR(c)
    case c::s => SEQ(CHAR(c), charlist2rexp(s))
}

implicit def string2rexp(s: String) : Rexp = charlist2rexp(s.toList)

def STAR(r: Rexp) : Rexp = FROM(r, 0)
def PLUS(r: Rexp) : Rexp = FROM(r, 1)
def OPTIONAL(r: Rexp) : Rexp = ALT(r, ONE)
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

def lex(r: ARexp, s: List[Char]) : List[Boolean] = s match {
    case Nil => if(nullable(r)) mkeps(r) else throw new Exception("Not matched")
    case c::cs => lex(simp(der(r, c)), cs)
}

def lexer(r: Rexp, s: String) : List[Boolean] = lex(internalise(r), s.toList)

def flatten(v: Val) : String = v match {
    case Empty => ""
    case Chr(c) => c.toString
    case Left(v) => flatten(v)
    case Right(v) => flatten(v)
    case Sequ(v1, v2) => flatten(v1) + flatten(v2)
    case From(vs) => vs.map(flatten).mkString
    case Between(vs) => vs.map(flatten).mkString
    case Not(v) => flatten(v)
}

def getString(r: Rexp, bs: List[Boolean]) = flatten(decode(r, bs))

def size(r: ARexp) : Int = r match {
    case AZERO => 1
    case AONE(_) => 1
    case ACHAR(_, _) => 1
    case AALT(_, ls) => 1 + ls.map(size(_)).sum
    case ASEQ(_, r1, r2) => 1 + size(r1) + size(r2)
    case AFROM(_, r, n) => 1 + size(r)
    case ABETWEEN(_, r, n, m) => 1 + size(r)
    case ANOT(_, r) => 1 + size(r)
}

// for measuring time
def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  (end - start) / (i * 1.0e9)
}

def test(r: Rexp, s: String) = {
    val bs = lexer(r, s)
    assert(s == getString(r, bs))
}

///tests

val reg0 = ("a"%(1, 2) | "x")>2
val reg1 = ("a"|"e")%
val reg2 = ((("a"%(2, 4))?) ~ ("c" | "d"))%
val reg3 = (((("a")?)%(2,3))~("b"|"c"))%
val reg3_1 = (((("a")?)%(2,3))~("b"))%
val reg4 = ((("ab")?)%(1,2))%
val reg5 = (((("ab")?)%(1,2)) ~ "c")%
val reg6 = ((("a")?) ~ ("b" | "c"))
val reg7 = (("a"%(1, 2)) | ("b"%(1, 2)))%
val reg8 = ("a"|"c")%
val reg9 = (("a") ~ ("b" | "c"))%
val reg10 = (("a"?) ~ ("b" | "c"))%
val reg11 = ((("a">2)?) ~ ("b" | "c"))%
val reg12 = (("a">2) ~ "b")%
val reg13 = (("a"%(2,3)) ~ ("b" | "c"))%

val r1_s1 = "aaaeeeaeaeaeaaeee"
val r2_s1 = "aaaac"



