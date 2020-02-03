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

abstract class Bit
case object S extends Bit
case object Z extends Bit

type Bits = List[Bit]

implicit def bit2bits(b: Bit) : Bits = List(b)

abstract class ARexps
case object AZEROs extends ARexps
case class AONEs(bs: Bits) extends ARexps
case class ACHARs(bs: Bits, c: Char) extends ARexps
case class AALTs(bs: Bits, l: List[ARexps]) extends ARexps
case class ASEQs(bs: Bits, r1: ARexps, r2: ARexps) extends ARexps
case class AFROMs(bs: Bits, r: ARexps, n: Int) extends ARexps
case class ABETWEENs(bs: Bits, r: ARexps, n: Int, m: Int) extends ARexps
case class ANOTs(bs: Bits, r: ARexps) extends ARexps

abstract class Val
case object Empty extends Val
case class Chr(c: Char) extends Val
case class Sequ(c1: Val, v2: Val) extends Val
case class Left(v: Val) extends Val
case class Right(v: Val) extends Val
case class From(vs: List[Val]) extends Val
case class Between(vs: List[Val]) extends Val
case class Not(v: Val) extends Val

def decode_aux(r: Rexp, bs: Bits) : (Val, Bits) = (r, bs) match {
    case (ONE, bs) => (Empty, bs)
    case (CHAR(c), bs) => (Chr(c), bs)
    case (ALT(r1, r2), Z::bs) => {
        val (v, bs1) = decode_aux(r1, bs)
        (Left(v), bs1)
    }
    case (ALT(r1, r2), S::bs) => {
        val (v, bs1) = decode_aux(r2, bs)
        (Right(v), bs1)
    }
    case (SEQ(r1, r2), bs) => {
        val (v1, bs1) = decode_aux(r1, bs)
        val (v2, bs2) = decode_aux(r2, bs1)
        (Sequ(v1, v2), bs2)
    }
    case (FROM(r,_), Z::bs) => {
        val (v, bs1) = decode_aux(r, bs)
        val (From(vs), bs2) = decode_aux(FROM(r,0), bs1)
        (From(v::vs), bs2)
    }
    case (FROM(_,_), S::bs) => (From(Nil), bs)
    case (BETWEEN(r,_,_), Z::bs) => {
        val (v, bs1) = decode_aux(r, bs)
        val (Between(vs), bs2) = decode_aux(BETWEEN(r,0,0), bs1)
        (Between(v::vs), bs2)
    }
    case (BETWEEN(_,_,_), S::bs) => (Between(Nil), bs)
    case (NOT(r), bs) => {
        val (v, bs1) = decode_aux(r, bs)
        (Not(v), bs1)
    }
}

def decode(r: Rexp, bs: Bits) = decode_aux(r, bs) match {
    case (v, Nil) => v
    case _ => throw new Exception("Not decodable")
}

def nullable(r: ARexps) : Boolean = r match {
    case AZEROs => false
    case AONEs(_) => true
    case ACHARs(_,_) => false
    case AALTs(bs, ls) => ls match {
        case Nil => false
        case head::tail => nullable(head) || nullable(AALTs(bs, tail))
    }
    case ASEQs(_, r1, r2) => nullable(r1) && nullable(r2)
    case AFROMs(_,r, i) => if(i == 0) true else nullable(r)
    case ABETWEENs(_,r, i, _) => if(i == 0) true else nullable(r)
    case ANOTs(_,r) => !nullable(r)
}

def fuse_reversed(bs: Bits, r: ARexps) : ARexps = r match {
    case AZEROs => AZEROs
    case AONEs(bsq) => AONEs(bsq ++ bs)
    case ACHARs(bsq, c) => ACHARs(bsq ++ bs, c)
    case AALTs(bsq, ls) => AALTs(bsq ++ bs, ls)
    case ASEQs(bsq, r1, r2) => ASEQs(bsq ++ bs, r1, r2)
    case AFROMs(bsq, r, n) => AFROMs(bsq++bs, r, n)
    case ABETWEENs(bsq, r, n, m) => ABETWEENs(bsq++bs, r, n, m)
    case ANOTs(bsq, r) => ANOTs(bsq++bs, r)
}

def fuse(bs: Bits, r: ARexps) : ARexps = r match {
    case AZEROs => AZEROs
    case AONEs(bsq) => AONEs(bs ++ bsq)
    case ACHARs(bsq, c) => ACHARs(bs ++ bsq, c)
    case AALTs(bsq, ls) => AALTs(bs ++ bsq, ls)
    case ASEQs(bsq, r1, r2) => ASEQs(bs ++ bsq, r1, r2)
    case AFROMs(bsq, r, n) => AFROMs(bs++bsq, r, n)
    case ABETWEENs(bsq, r, n, m) => ABETWEENs(bs++bsq, r, n, m)
    case ANOTs(bsq, r) => ANOTs(bs++bsq, r)
}

def delete(r: ARexps): Rexp = r match{
    case AZEROs => ZERO
    case AONEs(_) => ONE
    case ACHARs(_, c) => CHAR(c)
    case AALTs(bs, Nil) => ZERO
    case AALTs(bs, head::Nil) => delete(head)
    case AALTs(bs, head1::head2::tail) => ALT(delete(head1), delete(head2))
    case AALTs(bs, head::tail) => ALT(delete(head), delete(AALTs(bs, tail)))
    case ASEQs(_, r1, r2) => SEQ(delete(r1), delete(r2))
    case AFROMs(bsq, r, n) => FROM(delete(r), n)
    case ABETWEENs(bsq, r, n, m) => BETWEEN(delete(r), n, m)
    case ANOTs(bsq, r) => NOT(delete(r))
}

def mkeps(r: ARexps) : Bits = r match {
    case AONEs(bs) => bs
    case AALTs(bs, ls) => val matched = ls.find(x => nullable(x)).getOrElse(AZEROs); bs ++ mkeps(matched)
    case ASEQs(bs, r1, r2) => bs ++ mkeps(r1) ++ mkeps(r2)
    case AFROMs(bs, r, n) => bs ++ S
    case ABETWEENs(bs, r, n, m) => bs ++ S
    case ANOTs(bs, r) => bs ++ S
}

def der(r: ARexps, c: Char) : ARexps = r match {
    case AZEROs => AZEROs
    case AONEs(_) => AZEROs
    case ACHARs(bs, d) => if(d == c) AONEs(bs) else AZEROs
    case AALTs(bs, ls) => AALTs(bs, ls.map(x => der(x, c)))
    case ASEQs(bs, r1, r2) => if(nullable(r1)) 
                                 AALTs(bs, List(ASEQs(List(), der(r1, c), r2), fuse(mkeps(r1), der(r2, c))))
                            else ASEQs(bs, der(r1, c), r2)
    case AFROMs(bs, r, i) =>
        if(i == 0) ASEQs(bs, fuse(Z, der(r, c)), AFROMs(List(), r, 0)) 
        else ASEQs(bs, fuse(Z, der(r, c)), AFROMs(List(), r, i-1))
    case ABETWEENs(bs, r, i, j) => (i, j) match {
        case(0, 0) => AZEROs
        case(0, i) => ASEQs(bs, fuse(Z, der(r, c)), ABETWEENs(List(), r, 0, i - 1))
        case(i, j) => ASEQs(bs, fuse(Z, der(r, c)), ABETWEENs(List(), r, i - 1, j - 1))
    }
    case ANOTs(bs, r) => ANOTs(bs, der(r, c))
}

def rmvZeroDup(ls: List[ARexps], set: Set[Rexp]): List[ARexps] = ls match{
    case Nil => List()
    case head::Nil =>  head match {
        case AZEROs => List()
        case _ => if(!(set contains delete(head))) List(head) 
                    else List()
    }
    case head::tail => head match {
        case AZEROs => rmvZeroDup(tail, set)
        case _ => if(!(set contains delete(head)))
            List(head) ++ rmvZeroDup(tail, set ++ Set(delete(head)))
            else rmvZeroDup(tail, set)
    }
}

def getLs(r: ARexps): List[ARexps] = r match {
    case AALTs(bs, ls) => flatten_alt(ls.map(e => fuse(bs, e)))
    case r => List(r)
}

def flatten_alt(ls: List[ARexps]) : List[ARexps] = ls match {
    case Nil => List()
    case head::Nil => getLs(head)
    case head::tail => getLs(head) ++ flatten_alt(tail)
}

def emptyBsq(r: ARexps) : ARexps = r match {
    case AZEROs => AZEROs
    case AONEs(_) => AONEs(List())
    case ACHARs(_, c) => ACHARs(List(), c)
    case AALTs(_, ls) => AALTs(List(), ls)
    case ASEQs(_, r1, r2) => ASEQs(List(), r1, r2)
    case AFROMs(bsq, r, n) => AFROMs(List(), r, n)
    case ABETWEENs(bsq, r, n, m) => ABETWEENs(List(), r, n, m)
    case ANOTs(bsq, r) => ANOTs(List(), r)
}

def getBsq(r: ARexps) = r match{
    case AZEROs => List()
    case AONEs(bsq) => bsq
    case ACHARs(bsq, _) => bsq
    case AALTs(bsq, _) => bsq
    case ASEQs(bsq, _, _) => bsq
    case AFROMs(bsq, r, n) => bsq
    case ABETWEENs(bsq, r, n, m) => bsq
    case ANOTs(bsq, r) => bsq
}

def multiplyRight(ls: List[ARexps], r: ARexps) : List[ARexps] = {
    for(r1 <- ls) yield ASEQs(getBsq(r1), emptyBsq(r1), r)
}

def multiplyLeft(r: ARexps, ls: List[ARexps]) : List[ARexps] = {
   for(r1 <- ls) yield ASEQs(getBsq(r1), r, emptyBsq(r1))
}

def multiply(ls1: List[ARexps], ls2: List[ARexps]) = {
    for(r1 <- ls1; r2 <- ls2) yield ASEQs(getBsq(r1)++getBsq(r2), emptyBsq(r1), emptyBsq(r2))
}

def simp(r: ARexps) : ARexps = r match {
    case ASEQs(bs, r1, r2) =>  (simp(r1), simp(r2)) match {
        case (AZEROs, _) => AZEROs
        case (_, AZEROs) => AZEROs
        case (AONEs(bs1), r2s) => fuse(bs++bs1, r2s)
        case (r1s, AONEs(bs1)) => fuse(bs, fuse_reversed(bs1, r1s))
        case (AALTs(bs1, ls1), AALTs(bs2, ls2)) => AALTs(bs++bs1++bs2, multiply(ls1, ls2))
        case (AALTs(bs1, ls), r2s) => AALTs(bs++bs1, multiplyRight(ls, r2s))
        case (r1s, AALTs(bs1, ls)) => AALTs(bs++bs1, multiplyLeft(r1s, ls))
        case (r1s, r2s) => ASEQs(bs, r1s, r2s)
    }
    case AALTs(bs, ls) => ls match {
        case Nil => AZEROs
        case head::Nil => fuse(bs, head)
        case head::tail => {
            val newL = rmvZeroDup(flatten_alt(List(head)++tail), Set()).map(simp(_))
            AALTs(bs, newL)
        }
    }
    //case AFROMs(bs, r, n) => AFROMs(bs, simp(r), n)
    //case ABETWEENs(bs, r, n, m) => ABETWEENs(bs, simp(r), n, m)
    case r => r
}

def internalise(r: Rexp) : ARexps = r match {
    case ZERO => AZEROs
    case ONE => AONEs(List())
    case CHAR(c) => ACHARs(List(), c)
    case ALT(r1, r2) => AALTs(List(), List(fuse(Z, internalise(r1)), 
                                                    fuse(S, internalise(r2))))
    case SEQ(r1, r2) => ASEQs(List(), internalise(r1), internalise(r2))
    case FROM(r, n) => AFROMs(List(), internalise(r), n)
    case BETWEEN(r, n, m) => ABETWEENs(List(), internalise(r), n, m)
    case NOT(r) => ANOTs(List(), internalise(r))
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

def lex(r: ARexps, s: List[Char]) : Bits = s match {
    case Nil => if(nullable(r)) mkeps(r) else throw new Exception("Not matched")
    case c::cs => lex(simp(der(r, c)), cs)
}

def lexer(r: Rexp, s: String) : Bits = lex(internalise(r), s.toList)

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

def getString(r: Rexp, bs: Bits) = flatten(decode(r, bs))

def size(r: ARexps) : Int = r match {
    case AZEROs => 1
    case AONEs(_) => 1
    case ACHARs(_, _) => 1
    case AALTs(_, ls) => 1 + ls.map(size(_)).sum
    case ASEQs(_, r1, r2) => 1 + size(r1) + size(r2)
    case AFROMs(_, r, n) => 1 + size(r)
    case ABETWEENs(_, r, n, m) => 1 + size(r)
    case ANOTs(_, r) => 1 + size(r)
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

//(a{1,2} + x){2, ..}
val reg0 = ("a"%(1, 2) | "x")>2
test(reg0, "aaxaa")
test(reg0, "xx")
test(reg0, "aaa")

//(a + e)*
val reg1 = ("a"|"e")%
test(reg1, "aaaeee")
test(reg1, "")
test(reg1, "aa")
test(reg1, "ee")

//( a{2,4}? ~ (c+d))*
val reg2 = ((("a"%(2, 4))?) ~ ("c" | "d"))%
test(reg2, "aacdaaad")
test(reg2, "cdcdcdcddd")
test(reg2, "aac")

//(a{2,..} ~ b)*
val reg12 = (("a">2) ~ "b")%
test(reg12, "aab")

val r1_s1 = "aaaeeeaeaeaeaaeee"
val r2_s1 = "aaaac"


