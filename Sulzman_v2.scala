import scala.language.implicitConversions
import scala.language.reflectiveCalls
import scala.annotation.tailrec 
import scala.language.postfixOps  

abstract class Rexp
case object ZERO extends Rexp
case object ONE extends Rexp
case class CHAR(c: Char) extends Rexp
case class RANGE(ls: List[Char]) extends Rexp
case class ALT(r1: Rexp, r2: Rexp) extends Rexp 
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp 
case class STAR(r: Rexp) extends Rexp

abstract class Bit
case object S extends Bit
case object Z extends Bit

type Bits = List[Bit]

implicit def bit2bits(b: Bit) : Bits = List(b)

abstract class ARexp
case object AZERO extends ARexp
case class AONE(bs: Bits) extends ARexp
case class ACHAR(bs: Bits, c: Char) extends ARexp
case class ARANGE(bs: Bits, ls: List[Char]) extends ARexp
case class AALTs(bs: Bits, l: List[ARexp]) extends ARexp
case class ASEQ(bs: Bits, r1: ARexp, r2: ARexp) extends ARexp
case class ASTAR(bs: Bits, r: ARexp) extends ARexp

def AALT(bs: Bits, r1: ARexp, r2: ARexp) : ARexp = AALTs(bs, List(r1, r2))

abstract class Val
case object Empty extends Val
case class Chr(c: Char) extends Val
case class Sequ(c1: Val, v2: Val) extends Val
case class Left(v: Val) extends Val
case class Right(v: Val) extends Val
case class Star(vs: List[Val]) extends Val

def nullable(r: ARexp) : Boolean = r match {
    case AZERO => false
    case AONE(_) => true
    case ACHAR(_,_) => false
    case ARANGE(_,_) => false
    case AALTs(bs , ls) => ls match {
        case Nil => false
        case head::tail => nullable(head) || nullable(AALTs(bs, tail))
    }
    case ASEQ(_, r1, r2) => nullable(r1) && nullable(r2)
    case ASTAR(_,_) => true
}

def fuse_reversed(bs: Bits, r: ARexp) : ARexp = r match {
    case AZERO => AZERO
    case AONE(bsq) => AONE(bsq ++ bs)
    case ACHAR(bsq, c) => ACHAR(bsq ++ bs, c)
    case AALTs(bsq, ls) => AALTs(bsq ++ bs, ls)
    case ASEQ(bsq, r1, r2) => ASEQ(bsq ++ bs, r1, r2)
    case ASTAR(bsq, r1) => ASTAR(bsq++bs, r1)
}

def fuse(bs: Bits, r: ARexp) : ARexp = r match {
    case AZERO => AZERO
    case AONE(bsq) => AONE(bs ++ bsq)
    case ACHAR(bsq, c) => ACHAR(bs ++ bsq, c)
    case AALTs(bsq, ls) => AALTs(bs ++ bsq, ls)
    case ASEQ(bsq, r1, r2) => ASEQ(bs ++ bsq, r1, r2)
    case ASTAR(bsq, r1) => ASTAR(bs ++ bsq, r1)
}

def erase(r: ARexp): Rexp = r match{
    case AZERO => ZERO
    case AONE(_) => ONE
    case ACHAR(_, c) => CHAR(c)
    case AALTs(bs, Nil) => ZERO
    case AALTs(bs, head::Nil) => erase(head)
    case AALTs(bs, head::tail) => ALT(erase(head), erase(AALTs(bs, tail)))
    case ASEQ(_, r1, r2) => SEQ(erase(r1), erase(r2))
    case ASTAR(_, r1) => STAR(erase(r1))
}

def mkeps(r: ARexp) : Bits = r match {
    case AONE(bs) => bs
    case AALTs(bs, ls) => val matched = ls.find(x => nullable(x)).getOrElse(AZERO); bs ++ mkeps(matched)
    case ASEQ(bs, r1, r2) => bs ++ mkeps(r1) ++ mkeps(r2)
    case ASTAR(bs, r1) => bs ++ S
}

def der(r: ARexp, c: Char) : ARexp = r match {
    case AZERO => AZERO
    case AONE(_) => AZERO
    case ACHAR(bs, d) => if(d == c) AONE(bs) else AZERO
    case AALTs(bs, ls) => AALTs(bs, ls.map(x => der(x, c)))
    case ASEQ(bs, r1, r2) => if(nullable(r1)) 
                                 AALTs(bs, List(ASEQ(List(), der(r1, c), r2), fuse(mkeps(r1), der(r2, c))))
                            else ASEQ(bs, der(r1, c), r2)
    case ASTAR(bs, r1) => ASEQ(bs, fuse(Z, der(r1, c)), ASTAR(List(), r1))
}

def emptyBsq(r: ARexp) : ARexp = r match {
    case AZERO => AZERO
    case AONE(_) => AONE(List())
    case ACHAR(_, c) => ACHAR(List(), c)
    case AALTs(_, ls) => AALTs(List(), ls)
    case ASEQ(_, r1, r2) => ASEQ(List(), r1, r2)
    case ASTAR(_, r1) => ASTAR(List(), r1)
}

def getBsq(r: ARexp) = r match{
    case AZERO => List()
    case AONE(bsq) => bsq
    case ACHAR(bsq, _) => bsq
    case AALTs(bsq, _) => bsq
    case ASEQ(bsq, _, _) => bsq
    case ASTAR(bsq, _) => bsq
}

def multiplyRight(ls: List[ARexp], r: ARexp) : List[ARexp] = {
    for(r1 <- ls) yield ASEQ(getBsq(r1)++getBsq(r), emptyBsq(r1), emptyBsq(r))
}

def multiplyLeft(r: ARexp, ls: List[ARexp]) : List[ARexp] = {
   for(r1 <- ls) yield ASEQ(getBsq(r)++getBsq(r1), emptyBsq(r), emptyBsq(r1))
}

def multiply(ls1: List[ARexp], ls2: List[ARexp]) = {
    for(r1 <- ls1; r2 <- ls2) yield ASEQ(getBsq(r1)++getBsq(r2), emptyBsq(r1), emptyBsq(r2))
}

def simp_list(rs: List[ARexp]) : List[ARexp] = rs match {
    case Nil => Nil
    case AZERO :: rs => simp_list(rs)
    case AALTs(bs, rs1) :: rs => rs1.map(fuse(bs, _)) ++ simp_list(rs)
    case r1 :: rs => r1 :: simp_list(rs)
}

def distinctBy[B, C](xs: List[B], 
                     f: B => C, 
                     acc: List[C] = Nil): List[B] = xs match {
  case Nil => Nil
  case x::xs => {
    val res = f(x)
    if (acc.contains(res)) distinctBy(xs, f, acc)
    else x::distinctBy(xs, f, res::acc)
  }
} 

def simp(r: ARexp) : ARexp = r match {
    case ASEQ(bs, r1, r2) =>  (simp(r1), simp(r2)) match {
        case (AZERO, _) => AZERO
        case (_, AZERO) => AZERO
        case (AONE(bs1), r2s) => fuse(bs++bs1, r2s)
        case (r1s, AONE(bs1)) => fuse(bs, fuse_reversed(bs1, r1s))
        case (AALTs(bs1, ls), r2s) => AALTs(bs++bs1, multiplyRight(ls, r2s))
        case (r1s, AALTs(bs1, ls)) => AALTs(bs++bs1, multiplyLeft(r1s, ls))
        case (r1s, r2s) => ASEQ(bs, r1s, r2s)
    }
    case AALTs(bs, ls) => distinctBy(simp_list(ls.map(simp)), erase) match {
        case Nil => AZERO
        case head::Nil => fuse(bs, head)
        case ls => AALTs(bs, ls)
    }
    case ASTAR(bs, r1) => ASTAR(bs, simp(r1))
    case r => r
}

def internalise(r: Rexp) : ARexp = r match {
    case ZERO => AZERO
    case ONE => AONE(List())
    case CHAR(c) => ACHAR(List(), c)
    case ALT(r1, r2) => AALTs(List(), List(fuse(Z, internalise(r1)), 
                                                    fuse(S, internalise(r2))))
    case SEQ(r1, r2) => ASEQ(List(), internalise(r1), internalise(r2))
    case STAR(r1) => ASTAR(List(), internalise(r1))
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
    def | (r: Rexp) = ALT(s, r)
    def | (r: String) = ALT(s, r)
    def ~ (r: Rexp) = SEQ(s, r)
    def ~ (r: String) = SEQ(s, r)
    def % = STAR(s)
}

@tailrec
def lex(r: ARexp, s: List[Char]) : Bits = s match {
    case Nil => if(nullable(r)) mkeps(r) else throw new Exception("Not matched")
    case c::cs => lex(simp(der(r, c)), cs)
}

def lexer(r: Rexp, s: String) : Bits = lex(simp(internalise(erase(simp(internalise(r))))), s.toList)

def flatten(v: Val) : String = v match {
    case Empty => ""
    case Chr(c) => c.toString
    case Left(v) => flatten(v)
    case Right(v) => flatten(v)
    case Sequ(v1, v2) => flatten(v1) + flatten(v2)
    case Star(vs) => vs.map(flatten).mkString
}


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
    case (STAR(r), Z::bs) => {
        val (v, bs1) = decode_aux(r, bs)
        val (Star(vs), bs2) = decode_aux(STAR(r), bs1)
        (Star(v::vs), bs2)
    }
    case (STAR(_), S::bs) => (Star(Nil), bs)
}

def decode(r: Rexp, bs: Bits) = decode_aux(erase(simp(internalise(r))), bs) match {
    case (v, Nil) => v
    case _ => throw new Exception("Not decodable")
}