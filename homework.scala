// A version with simplification of derivatives;
// this keeps the regular expressions small, which
// is good for the run-time

import scala.language.implicitConversions    
import scala.language.reflectiveCalls
import scala.annotation.tailrec   
 

abstract class Rexp
case object ZERO extends Rexp
case object ONE extends Rexp
case class CFUN(f: Char => Boolean) extends Rexp
case class ALT(r1: Rexp, r2: Rexp) extends Rexp 
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp 
case class STAR(r: Rexp) extends Rexp 
case class PLUS(r: Rexp) extends Rexp
case class OPTIONAL(r: Rexp) extends Rexp
case class NTIMES(r: Rexp, n: Int) extends Rexp
case class UPTO(r: Rexp, m: Int) extends Rexp
case class FROM(r: Rexp, n: Int) extends Rexp
case class BETWEEN(r: Rexp, n: Int, m: Int) extends Rexp
case class NOT(r: Rexp) extends Rexp

def ALL(c: Char) : Boolean = true

def CHAR(c1: Char): (Char) => Boolean = {
  (c2) => c1 == c2 
}

def RANGE(xs: Set[Char]): (Char) => Boolean = {
  (c) => xs.contains(c)
}

// the nullable function: tests whether the regular 
// expression can recognise the empty string
def nullable (r: Rexp) : Boolean = r match {
  case ZERO => false
  case ONE => true
  case CFUN(f) => false
  case ALT(r1, r2) => nullable(r1) || nullable(r2)
  case SEQ(r1, r2) => nullable(r1) && nullable(r2)
  case STAR(_) => true
  case NTIMES(r, i) => if (i == 0) true else nullable(r)
  case PLUS(r) => nullable(r)
  case OPTIONAL(_) => true
  case UPTO(_, _) => true
  case FROM(r, i) => if (i == 0) true else nullable(r)
  case BETWEEN(r, i, _) => if (i == 0) true else nullable(r)
  case NOT(r) => !nullable(r)
}

// the derivative of a regular expression w.r.t. a character
def der (c: Char, r: Rexp) : Rexp = r match {
  case ZERO => ZERO
  case ONE => ZERO
  case CFUN(f) => if (f(c)) ONE else ZERO
  case ALT(r1, r2) => ALT(der(c, r1), der(c, r2))
  case SEQ(r1, r2) =>
    if (nullable(r1)) ALT(SEQ(der(c, r1), r2), der(c, r2))
    else SEQ(der(c, r1), r2)
  case STAR(r1) => SEQ(der(c, r1), STAR(r1))
  case PLUS(r) => SEQ(der(c, r), STAR(r))
  case OPTIONAL(r) => der(c, r)
  case NTIMES(r, i) => 
    if (i == 0) ZERO else SEQ(der(c, r), NTIMES(r, i - 1))
  case UPTO(r, i) =>
    if (i == 0) ZERO else SEQ(der(c, r), UPTO(r, i - 1))
  case FROM(r, i) =>
    if(i == 0) SEQ(der(c,r), STAR(r)) else SEQ(der(c, r), FROM(r, i-1))
  case BETWEEN(r, i, j) => (i, j) match {
    case(0, 0) => ZERO
    case(0, i) => SEQ(der(c, r), UPTO(r, i - 1))
    case(i, j) => SEQ(der(c, r), BETWEEN(r, i - 1, j - 1))
  }
  case NOT(r) => NOT(der(c, r))
}

def simp(r: Rexp) : Rexp = r match {
  case ALT(r1, r2) => (simp(r1), simp(r2)) match {
    case (ZERO, r2s) => r2s
    case (r1s, ZERO) => r1s
    case (r1s, r2s) => if (r1s == r2s) r1s else ALT (r1s, r2s)
  }
  case SEQ(r1, r2) =>  (simp(r1), simp(r2)) match {
    case (ZERO, _) => ZERO
    case (_, ZERO) => ZERO
    case (ONE, r2s) => r2s
    case (r1s, ONE) => r1s
    case (r1s, r2s) => SEQ(r1s, r2s)
  }
  case r => r
}


// the derivative w.r.t. a string (iterates der)
def ders(s: List[Char], r: Rexp) : Rexp = s match {
  case Nil => r
  case c::s => ders(s, simp(der(c, r)))
}


// the main matcher function
def matcher(r: Rexp, s: String) : Boolean = 
  nullable(ders(s.toList, r))


val test1 = NTIMES(CFUN(CHAR('a')), 3)
val test2 = NTIMES(OPTIONAL(CFUN(CHAR('a'))), 3)
val test3 = UPTO(CFUN(CHAR('a')), 5)
val test4 = BETWEEN(CFUN(CHAR('a')), 3, 5)
val test5 = BETWEEN(OPTIONAL(CFUN(CHAR('a'))), 3, 5)
val test6 = FROM(CFUN(CHAR('a')), 3)
val test7 = PLUS(CFUN(CHAR('a')))
val test8 = SEQ(STAR(CFUN(CHAR('a'))), OPTIONAL(CFUN(CHAR('b'))))
val test9 = FROM(CFUN(CHAR('a')), 0)
val test10 = NOT(SEQ(CFUN(CHAR('a')), CFUN(CHAR('b'))))
val test11 = SEQ(CFUN(CHAR('a')), PLUS(CFUN(CHAR('b'))))
val test12 = UPTO(CFUN(CHAR('a')), 2)

val myEmail = "tudor-cristian.iures@kcl.ac.uk"

val emailChars = Set.range('a', 'z') ++ Set.range('0', '9') ++ Set('9', '.', '_', '-')

val emailRegex = 
  SEQ(PLUS(CFUN(emailChars)),
  SEQ(CFUN(CHAR('@')), 
  SEQ(PLUS(CFUN(emailChars)),
  SEQ(CFUN(CHAR('.')), BETWEEN(CFUN(Set.range('a', 'z')),2, 6)))))

val comment = "/* this is a comment /* asdf*/"

/* comment */ /*asf */

val commentRegex = 
  SEQ(CFUN(CHAR('/')), 
  SEQ(CFUN(CHAR('*')),
  SEQ(NOT(
    SEQ(STAR(CFUN(ALL)), 
    SEQ(CFUN(CHAR('*')), 
    SEQ(CFUN(CHAR('/')), STAR(CFUN(ALL)))))),
  SEQ(CFUN(CHAR('*')), CFUN(CHAR('/'))))))


val r1 = 
  SEQ(CFUN(CHAR('a')), 
  SEQ(CFUN(CHAR('a')), CFUN(CHAR('a'))))

val r2 = SEQ(BETWEEN(CFUN(CHAR('a')), 19, 19), OPTIONAL(CFUN(CHAR('a'))))

val rr1 = PLUS(PLUS(r1))
val rr2 = PLUS(PLUS(r2))

val Q5 = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

val Q6 = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

val Q7 = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

matcher(rr1, Q5)
matcher(rr2, Q5)


matcher(rr1, Q6)
matcher(rr2, Q6)

matcher(rr1, Q7)
matcher(rr2, Q7)

/* some convenience for typing in regular expressions
def charlist2rexp(s : List[Char]): Rexp = s match {
  case Nil => ONE
  case c::Nil => CFUN(CHAR(c))
  case c::s => SEQ(CFUN(CHAR(c)), charlist2rexp(s))
}
implicit def string2rexp(s : String) : Rexp = charlist2rexp(s.toList)

implicit def RexpOps(r: Rexp) = new {
  def | (s: Rexp) = ALT(r, s)
  def % = STAR(r)
  def ~ (s: Rexp) = SEQ(r, s)
  def ? = OPTIONAL(r)
  def ! = NOT(r)
}

implicit def stringOps(s: String) = new {
  def | (r: Rexp) = ALT(s, r)
  def | (r: String) = ALT(s, r)
  def % = STAR(s)
  def ~ (r: Rexp) = SEQ(s, r)
  def ~ (r: String) = SEQ(s, r)
  def ? = OPTIONAL(r)
  def ! = NOT(r)
}*/

