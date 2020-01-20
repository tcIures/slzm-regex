import scala.language.implicitConversions
import scala.language.reflectiveCalls
import scala.annotation.tailrec 
import scala.language.postfixOps  
import collection.immutable.ListMap
import scala.collection._
import mutable.ListBuffer

class FixedList[A](max: Int) extends Iterable[A] {

  val list: ListBuffer[A] = ListBuffer()

  def add (elem: A) : Unit = {
    if (list.size < max) {
      list.append(elem)
    }
  }

  override def foreach[U](f: A => U) = list.foreach(f)

  override def iterator = list.iterator

}

//Bitcoded version of Sulzman and Lu's algorithm
abstract class Rexp
case object ZERO extends Rexp
case object ONE extends Rexp
case class CHAR(c: Char) extends Rexp
case class ALT(r1: Rexp, r2: Rexp) extends Rexp
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp
case class STAR(r: Rexp) extends Rexp

abstract class ARexps
case object AZEROs extends ARexps
case class AONEs(bs: List[Boolean]) extends ARexps
case class ACHARs(bs: List[Boolean], c: Char) extends ARexps
case class AALTs(bs: List[Boolean], l: List[ARexps]) extends ARexps
case class ASEQs(bs: List[Boolean], r1: ARexps, r2: ARexps) extends ARexps
case class ASTARs(bs: List[Boolean], r: ARexps) extends ARexps

abstract class Val
case object Empty extends Val
case class Chr(c: Char) extends Val
case class Sequ(c1: Val, v2: Val) extends Val
case class Left(v: Val) extends Val
case class Right(v: Val) extends Val
case class Stars(vs: List[Val]) extends Val

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

def bnullable(r: ARexps) : Boolean = r match {
    case AZEROs => false
    case AONEs(_) => true
    case ACHARs(_,_) => false
    case AALTs(bs, ls) => ls match {
        case Nil => false
        case head::tail => bnullable(head) || bnullable(AALTs(bs, tail))
    }
    case ASEQs(_, r1, r2) => bnullable(r1) && bnullable(r2)
    case ASTARs(_,_) => true
}

def fuse_reversed(bs: List[Boolean], r: ARexps) : ARexps = r match {
    case AZEROs => AZEROs
    case AONEs(bsq) => AONEs(bsq ++ bs)
    case ACHARs(bsq, c) => ACHARs(bsq ++ bs, c)
    case AALTs(bsq, ls) => AALTs(bsq ++ bs, ls)
    case ASEQs(bsq, r1, r2) => ASEQs(bsq ++ bs, r1, r2)
    case ASTARs(bsq, r) => ASTARs(bsq ++ bs, r)    
}

def fuse(bs: List[Boolean], r: ARexps) : ARexps = r match {
    case AZEROs => AZEROs
    case AONEs(bsq) => AONEs(bs ++ bsq)
    case ACHARs(bsq, c) => ACHARs(bs ++ bsq, c)
    case AALTs(bsq, ls) => AALTs(bs ++ bsq, ls)
    case ASEQs(bsq, r1, r2) => ASEQs(bs ++ bsq, r1, r2)
    case ASTARs(bsq, r) => ASTARs(bs ++ bsq, r)
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
    case ASTARs(_, r) => STAR(delete(r))
}

def bmkeps(r: ARexps) : List[Boolean] = r match {
    case AONEs(bs) => bs
    case AALTs(bs, ls) => val matched = ls.find(x => bnullable(x)).getOrElse(AZEROs); bs ++ bmkeps(matched)
    case ASEQs(bs, r1, r2) => bs ++ bmkeps(r1) ++ bmkeps(r2)
    case ASTARs(bs, r) => bs ++ List(true)
}

def bder(r: ARexps, c: Char) : ARexps = r match {
    case AZEROs => AZEROs
    case AONEs(_) => AZEROs
    case ACHARs(bs, d) => if(d == c) AONEs(bs) else AZEROs
    case AALTs(bs, ls) => AALTs(bs, ls.map(x => bder(x, c)))
    case ASEQs(bs, r1, r2) => if(bnullable(r1)) 
                                AALTs(bs, List(ASEQs(List(), bder(r1, c), r2), fuse(bmkeps(r1), bder(r2, c))))
                            else ASEQs(bs, bder(r1, c), r2)
    case ASTARs(bs, r) => ASEQs(bs, fuse(List(false), bder(r, c)), ASTARs(List(), r))
}

def getBsq(r: ARexps) = r match{
    case AZEROs => List()
    case AONEs(bsq) => bsq
    case ACHARs(bsq, _) => bsq
    case AALTs(bsq, _) => bsq
    case ASEQs(bsq, _, _) => bsq
    case ASTARs(bsq, _) => bsq
}

def rmvZeroDup(ls: List[ARexps], set: Set[Rexp]): List[ARexps] = ls match{
    case Nil => List()
    case head::Nil =>  head match {
        case AZEROs => List()
        case _ => if(!(set contains delete(head))){
                    List(head) 
                } 
                    else List()
        //case _ => List(head)
    }
    case head::tail => head match {
        case AZEROs => rmvZeroDup(tail, set)
        case _ => if(!(set contains delete(head))){
            List(head) ++ rmvZeroDup(tail, set ++ Set(delete(head)))}
            else rmvZeroDup(tail, set)
        //case _ => List(head) ++ rmvZeroDup(tail, set)
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
    case ASTARs(_, ls) => ASTARs(List(), ls)
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
            val newL = rmvZeroDup(flatten_alt(List(head)++tail), Set())
            AALTs(bs, newL)
        }
    }

    case ASTARs(bs, r) => ASTARs(bs, simp(r))
    case r => r
}

def internalise(r: Rexp) : ARexps = r match {
    case ZERO => AZEROs
    case ONE => AONEs(List())
    case CHAR(c) => ACHARs(List(), c)
    case ALT(r1, r2) => AALTs(List(), List(fuse(List(false), internalise(r1)), 
                                                    fuse(List(true), internalise(r2))))
    case SEQ(r1, r2) => ASEQs(List(), internalise(r1), internalise(r2))
    case STAR(r) => ASTARs(List(), internalise(r))
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

/*@tailrec
def bders(r: Rexp, s: List[Char]) : ARexp = s match {
    case Nil => r
    case c::cs => bders(bder(r, c), cs)
}*/

def blex(r: ARexps, s: List[Char]) : List[Boolean] = s match {
    case Nil => if(bnullable(r)) bmkeps(r) else throw new Exception("Not matched")
    case c::cs => blex(simp(bder(r, c)), cs)
}

def blexer(r: Rexp, s: String) : List[Boolean] = blex(internalise(r), s.toList)

def blex_info(r: ARexps, s: List[Char], ls: FixedList[Int]) : FixedList[Int] = s match {
    case Nil => {
        if(bnullable(r)) {
            ls.add(size(r)) 
            ls
        }else throw new Exception("Not matched")
    }
    case c::cs => {
        var newLs = blex_info(simp(bder(r, c)), cs, ls)
        newLs.add(size(r))
        newLs
    }
}

def blexer_info(r: Rexp, s: String, n: Int) : FixedList[Int] = blex_info(internalise(r), s.toList, new FixedList[Int](n))

def flatten(v: Val) : String = v match {
    case Empty => ""
    case Chr(c) => c.toString
    case Left(v) => flatten(v)
    case Right(v) => flatten(v)
    case Sequ(v1, v2) => flatten(v1) + flatten(v2)
    case Stars(vs) => vs.map(flatten).mkString
}

def size(r: ARexps) : Int = r match {
    case AZEROs => 1
    case AONEs(_) => 1
    case ACHARs(_, _) => 1
    case AALTs(_, ls) => 1 + ls.map(size(_)).sum
    case ASEQs(_, r1, r2) => 1 + size(r1) + size(r2)
    case ASTARs(_, r) => 1 + size(r)
}

// for measuring time
def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  (end - start) / (i * 1.0e9)
}

val evil1 = (("a")%) ~ "b"
time_needed(1, blexer(evil0, "a"*19 + "b"))

blexer_info(evil1, "a"*50 + "b")
blexer_info(evil1, "a"*150 + "b")
blexer_info(evil1, "a"*250 + "b")
blexer_info(evil1, "a"*350 + "b")

for(i <- 1 to 1000 by 50) {
    println(blexer_info(evil1, "a"*i + "b"))
}


blexer_info(evil1, "a"*1500 + "b", 20)
blexer_info(evil1, "a"*2000 + "b", 20)
blexer_info(evil1, "a"*2500 + "b", 20)
blexer_info(evil1, "a"*3000 + "b", 20)

val reg0 = "abc"%
blexer_info(reg0, "abc"*300, 20)

val reg0 = "abcde"%
blexer_info(reg0, "abcde"*130, 30)


val reg0 = ("abcde"~"c")%
blexer_info(reg0, "abcdec"*200, 50)

val reg0 = (("ab"~"c") | "d")%
blexer_info(reg0, "abcdddabcdddabcabcdabcabc"*20, 50)


val reg0 = ("ab"%)%
blexer_info(reg0, "ab"*130, 30)



