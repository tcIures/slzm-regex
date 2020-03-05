import scala.language.implicitConversions
import scala.language.reflectiveCalls
import scala.annotation.tailrec 
import scala.language.postfixOps  

abstract class Rexp
case object ZERO extends Rexp
case object ONE extends Rexp
case class CFUN(f: Char => Boolean) extends Rexp
case class ALT(r1: Rexp, r2: Rexp) extends Rexp 
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp 
case class FROM(r: Rexp, n: Int) extends Rexp
case class BETWEEN(r: Rexp, n: Int, m: Int) extends Rexp
case class NOT(r: Rexp) extends Rexp

abstract class Bit
case object S extends Bit
case object Z extends Bit
case class C(c: Char) extends Bit

type Bits = List[Bit]

implicit def bit2bits(b: Bit) : Bits = List(b)

abstract class ARexp
case object AZERO extends ARexp
case class AONE(bs: Bits) extends ARexp
case class ACFUN(bs: Bits, f: Char => Boolean) extends ARexp
case class AALTs(bs: Bits, l: List[ARexp]) extends ARexp
case class ASEQ(bs: Bits, r1: ARexp, r2: ARexp) extends ARexp
case class AFROM(bs: Bits, r: ARexp, n: Int) extends ARexp
case class ABETWEEN(bs: Bits, r: ARexp, n: Int, m: Int) extends ARexp
case class ANOT(bs: Bits, r: ARexp) extends ARexp

def AALT(bs: Bits, r1: ARexp, r2: ARexp) : ARexp = AALTs(bs, List(r1, r2))

abstract class Val
case object Empty extends Val
case class Chr(c: Char) extends Val
case class Sequ(c1: Val, v2: Val) extends Val
case class Left(v: Val) extends Val
case class Right(v: Val) extends Val
case class From(vs: List[Val]) extends Val
case class Between(vs: List[Val]) extends Val
case class Not(v: Val) extends Val

def nullable(r: ARexp) : Boolean = r match {
    case AZERO => false
    case AONE(_) => true
    case ACFUN(_, _) => false
    case AALTs(bs , ls) => ls match {
        case Nil => false
        case head::tail => nullable(head) || nullable(AALTs(bs, tail))
    }
    case ASEQ(_, r1, r2) => nullable(r1) && nullable(r2)
    case AFROM(_,r, i) => if(i == 0) true else nullable(r)
    case ABETWEEN(_,r, i, _) => if(i == 0) true else nullable(r)
    case ANOT(_,r) => !nullable(r)
}

def fuse_reversed(bs: Bits, r: ARexp) : ARexp = r match {
    case AZERO => AZERO
    case AONE(bsq) => AONE(bsq ++ bs)
    case ACFUN(bsq, f) => ACFUN(bsq ++ bs, f)
    case AALTs(bsq, ls) => AALTs(bsq ++ bs, ls)
    case ASEQ(bsq, r1, r2) => ASEQ(bsq ++ bs, r1, r2)
    case AFROM(bsq, r, n) => AFROM(bsq++bs, r, n)
    case ABETWEEN(bsq, r, n, m) => ABETWEEN(bsq++bs, r, n, m)
    case ANOT(bsq, r) => ANOT(bsq++bs, r)
}

def fuse(bs: Bits, r: ARexp) : ARexp = r match {
    case AZERO => AZERO
    case AONE(bsq) => AONE(bs ++ bsq)
    case ACFUN(bsq, f) => ACFUN(bs ++ bsq, f)
    case AALTs(bsq, ls) => AALTs(bs ++ bsq, ls)
    case ASEQ(bsq, r1, r2) => ASEQ(bs ++ bsq, r1, r2)
    case AFROM(bsq, r, n) => AFROM(bs++bsq, r, n)
    case ABETWEEN(bsq, r, n, m) => ABETWEEN(bs++bsq, r, n, m)
    case ANOT(bsq, r) => ANOT(bs++bsq, r)
}

def erase(r: ARexp): Rexp = r match{
    case AZERO => ZERO
    case AONE(_) => ONE
    case ACFUN(_, f) => CFUN(f)
    case AALTs(bs, Nil) => ZERO
    case AALTs(bs, head::Nil) => erase(head)
    case AALTs(bs, head::tail) => ALT(erase(head), erase(AALTs(bs, tail)))
    case ASEQ(_, r1, r2) => SEQ(erase(r1), erase(r2))
    case AFROM(bsq, r, n) => FROM(erase(r), n)
    case ABETWEEN(bsq, r, n, m) => BETWEEN(erase(r), n, m)
    case ANOT(bsq, r) => NOT(erase(r))
}

def mkeps(r: ARexp) : Bits = r match {
    case AONE(bs) => bs
    case AALTs(bs, ls) => val matched = ls.find(x => nullable(x)).getOrElse(AZERO); bs ++ mkeps(matched)
    case ASEQ(bs, r1, r2) => bs ++ mkeps(r1) ++ mkeps(r2)
    case AFROM(bs, r, n) => bs ++ S
    case ABETWEEN(bs, r, n, m) => bs ++ S
    case ANOT(bs, r) => bs ++ mkeps(r)
}

def der(r: ARexp, c: Char) : ARexp = r match {
    case AZERO => AZERO
    case AONE(_) => AZERO
    case ACFUN(bs, f) => if(f(c)) AONE(bs ++ C(c)) else AZERO
    case AALTs(bs, ls) => AALTs(bs, ls.map(x => der(x, c)))
    case ASEQ(bs, r1, r2) => if(nullable(r1)) 
                                 AALTs(bs, List(ASEQ(List(), der(r1, c), r2), fuse(mkeps(r1), der(r2, c))))
                            else ASEQ(bs, der(r1, c), r2)
    case AFROM(bs, r, i) => {
        if(i == 0) ASEQ(bs, fuse(Z, der(r, c)), AFROM(List(), r, 0)) 
        else ASEQ(bs, fuse(Z, der(r, c)), AFROM(List(), r, i-1))
    }
    case ABETWEEN(bs, r, i, j) => (i, j) match {
        case(0, 0) => AZERO
        case(0, i) => ASEQ(bs, fuse(Z, der(r, c)), ABETWEEN(List(), r, 0, i - 1))
        case(i, j) => ASEQ(bs, fuse(Z, der(r, c)), ABETWEEN(List(), r, i - 1, j - 1))
    }
    case ANOT(bs, r) => r match {
        case ACFUN(bs, f) => if(!f(c)) AONE(bs ++ C(c)) else AZERO
        case ANOT(bs, r) => throw new Exception("Invalid Syntax")
    }
}

def emptyBsq(r: ARexp) : ARexp = r match {
    case AZERO => AZERO
    case AONE(_) => AONE(List())
    case ACFUN(_, f) => ACFUN(List(), f)
    case AALTs(_, ls) => AALTs(List(), ls)
    case ASEQ(_, r1, r2) => ASEQ(List(), r1, r2)
    case AFROM(bsq, r, n) => AFROM(List(), r, n)
    case ABETWEEN(bsq, r, n, m) => ABETWEEN(List(), r, n, m)
    case ANOT(bsq, r) => ANOT(List(), r)
}

def getBsq(r: ARexp) = r match{
    case AZERO => List()
    case AONE(bsq) => bsq
    case ACFUN(bsq, _) => bsq
    case AALTs(bsq, _) => bsq
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

def simp_bounded(r: ARexp) : Rexp = r match {
    case AFROM(bs, r1, n) => FROM(erase(r1), 0)
    case ABETWEEN(bs, r1, n, m) => BETWEEN(erase(r1), 0, 0)
    case _ => erase(r)
}

def getLowerBound(r: ARexp) : Int = r match {
    case AFROM(bs, r, n) => n
    case ABETWEEN(bs, r, n, m) => n
    case _ => 0
}

def getUpperBound(r: ARexp) : Int = r match {
    case ABETWEEN(bs, r, n, m) => m
    case _ => 0
}

def simp_list_aux(rs: List[ARexp], s: Set[ARexp]) : (List[ARexp], Set[ARexp]) = rs match {
    case Nil => (Nil, s)
    case AZERO :: rs => simp_list_aux(rs, s)
    case AALTs(bs, rs1) :: rs => {
        val (list, set) = simp_list_aux(rs, s)
        (rs1.map(fuse(bs, _)) ++ list , set)
    }
    case AFROM(bs, r, n) :: rs => s.find(simp_bounded(_) == FROM(erase(r), 0)) match {
        case Some(r1) => {
            val min = List(n, getLowerBound(r1)).min
            val newSet = (s -- Set(r1)) ++ Set(AFROM(bs, r, min))
            val (list, set) = simp_list_aux(rs, newSet)
            (AFROM(bs, r, min) :: list , set) 
        }
        case None => {
            val newSet = (s ++ Set(AFROM(bs, r, n)))
            val (list, set) = simp_list_aux(rs, newSet)
            (AFROM(bs, r, n) :: list, set)
        }
    }
    case ABETWEEN(bs, r, n, m) :: rs => s.find(simp_bounded(_) == BETWEEN(erase(r), 0, 0)) match {
        case Some(r1) => {
            val min = List(n, getLowerBound(r1)).min
            val max = List(m, getUpperBound(r1)).max
            val newSet = (s -- Set(r1)) ++ Set(ABETWEEN(bs, r, min, max))
            val (list, set) = simp_list_aux(rs, newSet)
            (ABETWEEN(bs, r, min, max) :: list , set) 
        }
        case None => {
            val newSet = (s ++ Set(ABETWEEN(bs, r, n, m)))
            val (list, set) = simp_list_aux(rs, newSet)
            (ABETWEEN(bs, r, n, m) :: list, set)
        }
    }
    case r1 :: rs => {
        val (list, set) = simp_list_aux(rs, s)
        (r1 :: list, set)
    }
}

def handle_bounded(rs: List[ARexp], s: Set[ARexp]) : List[ARexp] = rs match {
    case Nil => Nil
    case AFROM(bs, r, n) :: rs => s.find(simp_bounded(_) == FROM(erase(r), 0)) match {
        case Some(r1) => AFROM(bs, r, getLowerBound(r1)) :: handle_bounded(rs, s)
        case None => AFROM(bs, r, n) :: handle_bounded(rs, s)
    }
    case ABETWEEN(bs, r, n, m) :: rs => s.find(simp_bounded(_) == BETWEEN(erase(r), 0, 0)) match {
        case Some(r1) => ABETWEEN(bs, r, getLowerBound(r1), getUpperBound(r1)) :: handle_bounded(rs, s)
        case None => ABETWEEN(bs, r, n, m) :: handle_bounded(rs, s)
    }
    case r :: rs => r :: rs
}

def simp_list(rs: List[ARexp]) : List[ARexp] = {
    val (list, set) = simp_list_aux(rs, Set())
    handle_bounded(list, set)
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
        case (r1s, r2s) => ASEQ(bs, simp(r1s), simp(r2s))
    }
    case AALTs(bs, ls) => distinctBy(simp_list(ls.map(simp)), erase) match {
        case Nil => AZERO
        case head::Nil => fuse(bs, head)
        case ls => AALTs(bs, ls)
    }
    case AFROM(bs, r, n) => AFROM(bs, simp(r), n)
    case ABETWEEN(bs, r, n, m) => ABETWEEN(bs, simp(r), n, m)
    case r => r
}

def transform_aux(r: ARexp) : ARexp = r match {
    case ASEQ(bs, reg1, reg2) =>  (reg1, reg2) match {
        case (ABETWEEN(bs2, r2, n2, m2), ABETWEEN(bs3, r3, n3, m3)) =>
                    if(r2 == r3) 
                        ABETWEEN(bs2++bs3, transform_aux(r2), n2+n3, m2+m3) 
                    else ASEQ(bs, transform_aux(ABETWEEN(bs2, r2, n2, m2)),
                                 transform_aux(ABETWEEN(bs3, r3, n3, m3)))
        case (ABETWEEN(bs2, r2, n2, m2), AFROM(bs3, r3, n3)) => 
                    if(r2 == r3) 
                        AFROM(bs2++bs3, transform_aux(r2), n2+n3) 
                    else ASEQ(bs, ABETWEEN(bs2, transform_aux(r2), n2, m2), 
                                AFROM(bs3, transform_aux(r3), n3))
        case (AFROM(bs2, r2, n2), ABETWEEN(bs3, r3, n3, m3)) =>
                    if(r2 == r3) 
                        AFROM(bs2++bs3, transform_aux(r2), n2+n3) 
                    else ASEQ(bs, AFROM(bs2, transform_aux(r2), n2), 
                                ABETWEEN(bs3, transform_aux(r3), n3, m3))
        case (AFROM(bs2, r2, n2), AFROM(bs3, r3, n3)) =>
                    if(r2 == r3) 
                        AFROM(bs2++bs3, transform_aux(r2), n2+n3) 
                    else ASEQ(bs, AFROM(bs2, transform_aux(r2), n2), 
                                AFROM(bs3, transform_aux(r3), n3))
        case(_, _) => ASEQ(bs, transform_aux(reg1), transform_aux(reg2))
    }
    case AALTs(bs, rs) => AALTs(bs, rs.map(transform_aux(_)))
    case AFROM(bs, r1, n) => r1 match {
        //case AFROM(bs2, r2, n2) => AFROM(bs++bs2, transform_aux(r2), n*n2)
        //case ABETWEEN(bs2, r2, n2, _) => AFROM(bs++bs2, transform_aux(r2), n*n2)
        case _ => AFROM(bs, transform_aux(r1), n)
    }
    case ABETWEEN(bs, r1, n, m) => r1 match {
        //case AFROM(bs2, r2, n2) => AFROM(bs++bs2, transform_aux(r2), n*n2)
        //case ABETWEEN(bs2, r2, n2, m2) => ABETWEEN(bs++bs2, transform_aux(r2), n*n2, m*m2)
        case _ => ABETWEEN(bs, transform_aux(r1), n, m)
    }
    case r => r
}

def transform(r: ARexp) : ARexp = if(r == transform_aux(r)) r else transform(transform_aux(r))

def internalise(r: Rexp) : ARexp = r match {
    case ZERO => AZERO
    case ONE => AONE(List())
    case CFUN(f) => ACFUN(List(), f)
    case ALT(r1, r2) => AALTs(List(), List(fuse(Z, internalise(r1)), 
                                                    fuse(S, internalise(r2))))
    case SEQ(r1, r2) => ASEQ(List(), internalise(r1), internalise(r2))
    case FROM(r, n) => AFROM(List(), internalise(r), n)
    case BETWEEN(r, n, m) => ABETWEEN(List(), internalise(r), n, m)
    case NOT(r) => ANOT(List(), internalise(r))
}

def ALL_aux(c: Char) : Boolean = true
def CHAR_aux(c1: Char) : (Char) => Boolean = {
    (c2) => c1 == c2
}
def RANGE_aux(xs: Set[Char]) : (Char) => Boolean = {
    (c) => xs.contains(c)
}

def ALL() : Rexp = CFUN(ALL_aux)
def CHAR(c: Char) = CFUN(CHAR_aux(c))
def RANGE(xs: Set[Char]) : Rexp = CFUN(RANGE_aux(xs))

def charlist2rexp(s: List[Char]): Rexp = s match {
    case Nil => ONE
    case c::Nil => CHAR(c)
    case c::s => SEQ(CHAR(c), charlist2rexp(s))
}

implicit def string2rexp(s: String) : Rexp = charlist2rexp(s.toList)

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

@tailrec
def lex(r: ARexp, s: List[Char], counter: Int) : Bits = s match {
    case Nil => if(nullable(r)) mkeps(r) else throw new Exception("Not matched")
    case c::cs => if(counter % 100 == 0) lex(simp(der(r, c)), cs, counter + 1)
                    else lex((der(r, c)), cs, counter + 1)
}   

def normalize_aux(r: ARexp) : ARexp = if(simp(r) == r) r else normalize_aux(simp(r))

def normalize(r: ARexp) : ARexp = normalize_aux(r)

def preprocessing(r: Rexp) : Rexp = erase(normalize(transform(internalise(r))))
 
def lexer(r: Rexp, s: String) : Bits = lex((internalise(preprocessing(r))), s.toList, 0)

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

def decode_aux(r: Rexp, bs: Bits) : (Val, Bits) = (r, bs) match {
    case (ONE, bs) => (Empty, bs)
    case (CFUN(f), C(c)::bs) => (Chr(c), bs)
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

def decode(r: Rexp, bs: Bits) = decode_aux(preprocessing(r), bs) match {
    case (v, Nil) => v
    case _ => throw new Exception("Not decodable")
}