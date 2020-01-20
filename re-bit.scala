import scala.language.implicitConversions    
import scala.language.reflectiveCalls
import scala.annotation.tailrec   

abstract class Rexp 
case object ZERO extends Rexp
case object ONE extends Rexp
case class CHAR(c: Char) extends Rexp
case class ALT(r1: Rexp, r2: Rexp) extends Rexp 
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp 
case class STAR(r: Rexp) extends Rexp 


abstract class ARexp 
case object AZERO extends ARexp
case class AONE(bs: List[Boolean]) extends ARexp
case class ACHAR(bs: List[Boolean], c: Char) extends ARexp
case class AALT(bs: List[Boolean], r1: ARexp, r2: ARexp) extends ARexp 
case class ASEQ(bs: List[Boolean], r1: ARexp, r2: ARexp) extends ARexp 
case class ASTAR(bs: List[Boolean], r: ARexp) extends ARexp 

abstract class Val
case object Empty extends Val
case class Chr(c: Char) extends Val
case class Sequ(v1: Val, v2: Val) extends Val
case class Left(v: Val) extends Val
case class Right(v: Val) extends Val
case class Stars(vs: List[Val]) extends Val

   
// some convenience for typing in regular expressions
def charlist2rexp(s : List[Char]): Rexp = s match {
  case Nil => ONE
  case c::Nil => CHAR(c)
  case c::s => SEQ(CHAR(c), charlist2rexp(s))
}
implicit def string2rexp(s : String) : Rexp = charlist2rexp(s.toList)

implicit def RexpOps(r: Rexp) = new {
  def | (s: Rexp) = ALT(r, s)
  def % = STAR(r)
  def ~ (s: Rexp) = SEQ(r, s)
}

implicit def stringOps(s: String) = new {
  def | (r: Rexp) = ALT(s, r)
  def | (r: String) = ALT(s, r)
  def % = STAR(s)
  def ~ (r: Rexp) = SEQ(s, r)
  def ~ (r: String) = SEQ(s, r)
}


// nullable function: tests whether the regular 
// expression can recognise the empty string
def nullable (r: Rexp) : Boolean = r match {
  case ZERO => false
  case ONE => true
  case CHAR(_) => false
  case ALT(r1, r2) => nullable(r1) || nullable(r2)
  case SEQ(r1, r2) => nullable(r1) && nullable(r2)
  case STAR(_) => true
}

// derivative of a regular expression w.r.t. a character
def der (c: Char, r: Rexp) : Rexp = r match {
  case ZERO => ZERO
  case ONE => ZERO
  case CHAR(d) => if (c == d) ONE else ZERO
  case ALT(r1, r2) => ALT(der(c, r1), der(c, r2))
  case SEQ(r1, r2) => 
    if (nullable(r1)) ALT(SEQ(der(c, r1), r2), der(c, r2))
    else SEQ(der(c, r1), r2)
  case STAR(r) => SEQ(der(c, r), STAR(r))
}

// derivative w.r.t. a string (iterates der)
def ders (s: List[Char], r: Rexp) : Rexp = s match {
  case Nil => r
  case c::s => ders(s, der(c, r))
}

// mkeps and injection part
def mkeps(r: Rexp) : Val = r match {
  case ONE => Empty
  case ALT(r1, r2) => 
    if (nullable(r1)) Left(mkeps(r1)) else Right(mkeps(r2))
  case SEQ(r1, r2) => Sequ(mkeps(r1), mkeps(r2))
  case STAR(r) => Stars(Nil)
}


def inj(r: Rexp, c: Char, v: Val) : Val = (r, v) match {
  case (STAR(r), Sequ(v1, Stars(vs))) => Stars(inj(r, c, v1)::vs)
  case (SEQ(r1, r2), Sequ(v1, v2)) => Sequ(inj(r1, c, v1), v2)
  case (SEQ(r1, r2), Left(Sequ(v1, v2))) => Sequ(inj(r1, c, v1), v2)
  case (SEQ(r1, r2), Right(v2)) => Sequ(mkeps(r1), inj(r2, c, v2))
  case (ALT(r1, r2), Left(v1)) => Left(inj(r1, c, v1))
  case (ALT(r1, r2), Right(v2)) => Right(inj(r2, c, v2))
  case (CHAR(d), Empty) => Chr(c) 
}

// main lexing function (produces a value)
// - no simplification
def lex(r: Rexp, s: List[Char]) : Val = s match {
  case Nil => if (nullable(r)) mkeps(r) 
              else throw new Exception("Not matched")
  case c::cs => inj(r, c, lex(der(c, r), cs))
}

def lexing(r: Rexp, s: String) : Val = lex(r, s.toList)



// Bitcoded + Annotation
//=======================

// translation into ARexps
def fuse(bs: List[Boolean], r: ARexp) : ARexp = r match {
  case AZERO => AZERO
  case AONE(cs) => AONE(bs ++ cs)
  case ACHAR(cs, c) => ACHAR(bs ++ cs, c)
  case AALT(cs, r1, r2) => AALT(bs ++ cs, r1, r2)
  case ASEQ(cs, r1, r2) => ASEQ(bs ++ cs, r1, r2)
  case ASTAR(cs, r) => ASTAR(bs ++ cs, r)
}

def internalise(r: Rexp) : ARexp = r match {
  case ZERO => AZERO
  case ONE => AONE(Nil)
  case CHAR(c) => ACHAR(Nil, c)
  case ALT(r1, r2) => AALT(Nil, fuse(List(false), internalise(r1)), fuse(List(true), internalise(r2)))
  case SEQ(r1, r2) => ASEQ(Nil, internalise(r1), internalise(r2))
  case STAR(r) => ASTAR(Nil, internalise(r))
}


internalise(("a" | "ab") ~ ("b" | ""))

def retrieve(r: ARexp, v: Val) : List[Boolean] = (r, v) match {
  case (AONE(bs), Empty) => bs
  case (ACHAR(bs, c), Chr(d)) => bs
  case (AALT(bs, r1, r2), Left(v)) => bs ++ retrieve(r1, v)
  case (AALT(bs, r1, r2), Right(v)) => bs ++ retrieve(r2, v)
  case (ASEQ(bs, r1, r2), Sequ(v1, v2)) => 
    bs ++ retrieve(r1, v1) ++ retrieve(r2, v2)
  case (ASTAR(bs, r), Stars(Nil)) => bs ++ List(true)
  case (ASTAR(bs, r), Stars(v :: vs)) => 
     bs ++ List(false) ++ retrieve(r, v) ++ retrieve(ASTAR(Nil, r), Stars(vs))
}


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
  case (STAR(r1), false::bs) => {
    val (v, bs1) = decode_aux(r1, bs)
    val (Stars(vs), bs2) = decode_aux(STAR(r1), bs1)
    (Stars(v::vs), bs2)
  }
  case (STAR(_), true::bs) => (Stars(Nil), bs)
}

def decode(r: Rexp, bs: List[Boolean]) = decode_aux(r, bs) match {
  case (v, Nil) => v
  case _ => throw new Exception("Not decodable")
}

def encode(v: Val) : List[Boolean] = v match {
  case Empty => Nil
  case Chr(c) => Nil
  case Left(v) => false :: encode(v)
  case Right(v) => true :: encode(v)
  case Sequ(v1, v2) => encode(v1) ::: encode(v2)
  case Stars(Nil) => List(true)
  case Stars(v::vs) => false :: encode(v) ::: encode(Stars(vs))
}


// nullable function: tests whether the aregular 
// expression can recognise the empty string
def anullable (r: ARexp) : Boolean = r match {
  case AZERO => false
  case AONE(_) => true
  case ACHAR(_,_) => false
  case AALT(_, r1, r2) => anullable(r1) || anullable(r2)
  case ASEQ(_, r1, r2) => anullable(r1) && anullable(r2)
  case ASTAR(_, _) => true
}

def mkepsBC(r: ARexp) : List[Boolean] = r match {
  case AONE(bs) => bs
  case AALT(bs, r1, r2) => 
    if (anullable(r1)) bs ++ mkepsBC(r1) else bs ++ mkepsBC(r2)
  case ASEQ(bs, r1, r2) => bs ++ mkepsBC(r1) ++ mkepsBC(r2)
  case ASTAR(bs, r) => bs ++ List(true)
}

// derivative of a regular expression w.r.t. a character
def ader(c: Char, r: ARexp) : ARexp = r match {
  case AZERO => AZERO
  case AONE(_) => AZERO
  case ACHAR(bs, d) => if (c == d) AONE(bs) else AZERO
  case AALT(bs, r1, r2) => AALT(bs, ader(c, r1), ader(c, r2))
  case ASEQ(bs, r1, r2) => 
    if (anullable(r1)) AALT(bs, ASEQ(Nil, ader(c, r1), r2), fuse(mkepsBC(r1), ader(c, r2)))
    else ASEQ(bs, ader(c, r1), r2)
  case ASTAR(bs, r) => ASEQ(bs, fuse(List(false), ader(c, r)), ASTAR(Nil, r))
}

// derivative w.r.t. a string (iterates der)
@tailrec
def aders (s: List[Char], r: ARexp) : ARexp = s match {
  case Nil => r
  case c::s => aders(s, ader(c, r))
}

// main unsimplified lexing function (produces a value)
def alex(r: ARexp, s: List[Char]) : List[Boolean] = s match {
  case Nil => if (anullable(r)) mkepsBC(r) else throw new Exception("Not matched")
  case c::cs => alex(ader(c, r), cs)
}

def pre_alexing(r: ARexp, s: String)  : List[Boolean] = alex(r, s.toList)
def alexing(r: Rexp, s: String) : Val = decode(r, pre_alexing(internalise(r), s))


def asimp(r: ARexp): ARexp = r match {
  case ASEQ(bs1, r1, r2) => (asimp(r1), asimp(r2)) match {
      case (AZERO, _) => AZERO
      case (_, AZERO) => AZERO
      case (AONE(bs2), r2s) => fuse(bs1 ++ bs2, r2s)
      case (r1s, r2s) => ASEQ(bs1, r1s, r2s)
  }
  case AALT(bs1, r1, r2) => (asimp(r1), asimp(r2)) match {
      case (AZERO, r2s) => fuse(bs1, r2s)
      case (r1s, AZERO) => fuse(bs1, r1s)
      case (r1s, r2s) => AALT(bs1, r1s, r2s)
  }
  case r => r
}

def alex_simp(r: ARexp, s: List[Char]) : List[Boolean] = s match {
  case Nil => if (anullable(r)) mkepsBC(r) 
              else throw new Exception("Not matched")
  case c::cs => alex(asimp(ader(c, r)), cs)
}

def alexing_simp(r: Rexp, s: String) : Val = 
  decode(r, alex_simp(internalise(r), s.toList))



// extracts a string from value
def flatten(v: Val) : String = v match {
  case Empty => ""
  case Chr(c) => c.toString
  case Left(v) => flatten(v)
  case Right(v) => flatten(v)
  case Sequ(v1, v2) => flatten(v1) + flatten(v2)
  case Stars(vs) => vs.map(flatten).mkString
}

// extracts an environment from a value
def env(v: Val) : List[(String, String)] = v match {
  case Empty => Nil
  case Chr(c) => Nil
  case Left(v) => env(v)
  case Right(v) => env(v)
  case Sequ(v1, v2) => env(v1) ::: env(v2)
  case Stars(vs) => vs.flatMap(env)
}

// Some Tests
//============

def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  (end - start)/(i * 1.0e9)
}


val rf = ("a" | "ab") ~ ("ab" | "")
println(pre_alexing(internalise(rf), "ab"))
println(alexing(rf, "ab"))
println(alexing_simp(rf, "ab"))

val r0 = ("a" | "ab") ~ ("b" | "")
println(pre_alexing(internalise(r0), "ab"))
println(alexing(r0, "ab"))
println(alexing_simp(r0, "ab"))

val r1 = ("a" | "ab") ~ ("bcd" | "cd")
println(alexing(r1, "abcd"))
println(alexing_simp(r1, "abcd"))

println(alexing((("" | "a") ~ ("ab" | "b")), "ab"))
println(alexing_simp((("" | "a") ~ ("ab" | "b")), "ab"))

println(alexing((("" | "a") ~ ("b" | "ab")), "ab"))
println(alexing_simp((("" | "a") ~ ("b" | "ab")), "ab"))

println(alexing((("" | "a") ~ ("c" | "ab")), "ab"))
println(alexing_simp((("" | "a") ~ ("c" | "ab")), "ab"))


// Sulzmann's tests
//==================

val sulzmann = ("a" | "b" | "ab").%

println(alexing(sulzmann, "a" * 10))
println(alexing_simp(sulzmann, "a" * 10))

for (i <- 1 to 4001 by 500) {
  println(i + ": " + "%.5f".format(time_needed(1, alexing_simp(sulzmann, "a" * i))))
}

for (i <- 1 to 16 by 5) {
  println(i + ": " + "%.5f".format(time_needed(1, alexing_simp(sulzmann, "ab" * i))))
}




// some automatic testing

def clear() = {
  print("")
  //print("\33[H\33[2J")
}

// enumerates regular expressions until a certain depth
def enum(n: Int, s: String) : Stream[Rexp] = n match {
  case 0 => ZERO #:: ONE #:: s.toStream.map(CHAR)
  case n => {  
    val rs = enum(n - 1, s)
    rs #:::
    (for (r1 <- rs; r2 <- rs) yield ALT(r1, r2)) #:::
    (for (r1 <- rs; r2 <- rs) yield SEQ(r1, r2)) #:::
    (for (r1 <- rs) yield STAR(r1))
  }
}


//enum(2, "ab").size
//enum(3, "ab").size
//enum(3, "abc").size
//enum(4, "ab").size

import scala.util.Try

def test_mkeps(r: Rexp) = {
  val res1 = Try(Some(mkeps(r))).getOrElse(None)
  val res2 = Try(Some(decode(r, mkepsBC(internalise(r))))).getOrElse(None) 
  if (res1 != res2) println(s"Mkeps disagrees on ${r}")
  if (res1 != res2) Some(r) else (None)
}

println("Testing mkeps")
enum(2, "ab").map(test_mkeps).toSet
//enum(3, "ab").map(test_mkeps).toSet
//enum(3, "abc").map(test_mkeps).toSet


//enumerates strings of length n over alphabet cs
def strs(n: Int, cs: String) : Set[String] = {
  if (n == 0) Set("")
  else {
    val ss = strs(n - 1, cs)
    ss ++
    (for (s <- ss; c <- cs.toList) yield c + s)
  }
}

//tests lexing and lexingB
def tests_inj(ss: Set[String])(r: Rexp) = {
  clear()
  println(s"Testing ${r}")
  for (s <- ss.par) yield {
    val res1 = Try(Some(alexing(r, s))).getOrElse(None)
    val res2 = Try(Some(alexing_simp(r, s))).getOrElse(None)
    if (res1 != res2) println(s"Disagree on ${r} and ${s}")
    if (res1 != res2) println(s"   ${res1} !=  ${res2}")
    if (res1 != res2) Some((r, s)) else None
  }
}

//println("Testing lexing 1")
//enum(2, "ab").map(tests_inj(strs(2, "ab"))).toSet
//println("Testing lexing 2")
//enum(2, "ab").map(tests_inj(strs(3, "abc"))).toSet
//println("Testing lexing 3")
//enum(3, "ab").map(tests_inj(strs(3, "abc"))).toSet


def tests_alexer(ss: Set[String])(r: Rexp) = {
  clear()
  println(s"Testing ${r}")
  for (s <- ss.par) yield {
    val d = der('b', r)
    val ad = ader('b', internalise(r))
    val res1 = Try(Some(encode(inj(r, 'a', alexing(d, s))))).getOrElse(None)
    val res2 = Try(Some(pre_alexing(ad, s))).getOrElse(None)
    if (res1 != res2) println(s"Disagree on ${r} and 'a'::${s}")
    if (res1 != res2) println(s"   ${res1} !=  ${res2}")
    if (res1 != res2) Some((r, s)) else None
  }
}

println("Testing alexing 1")
println(enum(2, "ab").map(tests_alexer(strs(2, "ab"))).toSet)


def values(r: Rexp) : Set[Val] = r match {
  case ZERO => Set()
  case ONE => Set(Empty)
  case CHAR(c) => Set(Chr(c))
  case ALT(r1, r2) => (for (v1 <- values(r1)) yield Left(v1)) ++ 
                      (for (v2 <- values(r2)) yield Right(v2))
  case SEQ(r1, r2) => for (v1 <- values(r1); v2 <- values(r2)) yield Sequ(v1, v2)
  case STAR(r) => (Set(Stars(Nil)) ++ 
                  (for (v <- values(r)) yield Stars(List(v)))) 
    // to do more would cause the set to be infinite
}

def tests_ader(c: Char)(r: Rexp) = {
  val d = der(c, r)
  val vals = values(d)
  for (v <- vals) {
    println(s"Testing ${r} and ${v}")
    val res1 = retrieve(ader(c, internalise(r)), v)
    val res2 = encode(inj(r, c, decode(d, retrieve(internalise(der(c, r)), v))))
    if (res1 != res2) println(s"Disagree on ${r}, ${v} and der = ${d}")
    if (res1 != res2) println(s"   ${res1} !=  ${res2}")
    if (res1 != res2) Some((r, v)) else None
  }
}

println("Testing ader/der")
println(enum(2, "ab").map(tests_ader('a')).toSet)

val er = SEQ(ONE,CHAR('a')) 
val ev = Right(Empty) 
val ed = ALT(SEQ(ZERO,CHAR('a')),ONE)

retrieve(internalise(ed), ev) // => [true]
internalise(er)
ader('a', internalise(er))
retrieve(ader('a', internalise(er)), ev) // => []
decode(ed, List(true)) // gives the value for derivative
decode(er, List())     // gives the value for original value


val dr = STAR(CHAR('a'))
val dr_der = SEQ(ONE,STAR(CHAR('a'))) // derivative of dr
val dr_val = Sequ(Empty,Stars(List())) // value of dr_def


val res1 = retrieve(internalise(der('a', dr)), dr_val) // => [true]
val res2 = retrieve(ader('a', internalise(dr)), dr_val) // => [false, true]
decode(dr_der, res1) // gives the value for derivative
decode(dr, res2)     // gives the value for original value

encode(inj(dr, 'a', decode(dr_der, res1)))



