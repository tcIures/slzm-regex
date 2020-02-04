import scala.language.implicitConversions    
import scala.language.reflectiveCalls
import scala.annotation.tailrec  
import scala.language.postfixOps   

abstract class Rexp 
case object ZERO extends Rexp
case object ONE extends Rexp
case class CHAR(c: Char) extends Rexp
case class ALT(r1: Rexp, r2: Rexp) extends Rexp 
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp 
case class STAR(r: Rexp) extends Rexp 

abstract class Bit
case object Z extends Bit
case object S extends Bit

type Bits = List[Bit]

abstract class ARexp 
case object AZERO extends ARexp
case class AONE(bs: Bits) extends ARexp
case class ACHAR(bs: Bits, c: Char) extends ARexp
case class AALTS(bs: Bits, rs: List[ARexp]) extends ARexp 
case class ASEQ(bs: Bits, r1: ARexp, r2: ARexp) extends ARexp 
case class ASTAR(bs: Bits, r: ARexp) extends ARexp 

def AALT(bs: Bits, r1: ARexp, r2: ARexp) = AALTS(bs, List(r1, r2))

abstract class Val
case object Empty extends Val
case class Chr(c: Char) extends Val
case class Sequ(v1: Val, v2: Val) extends Val
case class Left(v: Val) extends Val
case class Right(v: Val) extends Val
case class Stars(vs: List[Val]) extends Val

   
// some convenience for typing in regular expressions
def charlist2rexp(s: List[Char]): Rexp = s match {
  case Nil => ONE
  case c::Nil => CHAR(c)
  case c::s => SEQ(CHAR(c), charlist2rexp(s))
}
implicit def string2rexp(s: String) : Rexp = charlist2rexp(s.toList)

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
def nullable(r: Rexp) : Boolean = r match {
  case ZERO => false
  case ONE => true
  case CHAR(_) => false
  case ALT(r1, r2) => nullable(r1) || nullable(r2)
  case SEQ(r1, r2) => nullable(r1) && nullable(r2)
  case STAR(_) => true
}

// derivative of a regular expression w.r.t. a character
def der(c: Char, r: Rexp) : Rexp = r match {
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
def ders(s: List[Char], r: Rexp) : Rexp = s match {
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

//erase function: extracts the regx from Aregex
def erase(r:ARexp): Rexp = r match{
  case AZERO => ZERO
  case AONE(_) => ONE
  case ACHAR(bs, c) => CHAR(c)
  case AALTS(bs, Nil) => ZERO
  case AALTS(bs, r::Nil) => erase(r)
  case AALTS(bs, r::rs) => ALT(erase(r), erase(AALTS(bs, rs)))
  case ASEQ(bs, r1, r2) => SEQ (erase(r1), erase(r2))
  case ASTAR(cs, r)=> STAR(erase(r))
}

// translation into ARexps
def fuse(bs: Bits, r: ARexp) : ARexp = r match {
  case AZERO => AZERO
  case AONE(cs) => AONE(bs ++ cs)
  case ACHAR(cs, c) => ACHAR(bs ++ cs, c)
  case AALTS(cs, rs) => AALTS(bs ++ cs, rs)
  case ASEQ(cs, r1, r2) => ASEQ(bs ++ cs, r1, r2)
  case ASTAR(cs, r) => ASTAR(bs ++ cs, r)
}

def internalise(r: Rexp) : ARexp = r match {
  case ZERO => AZERO
  case ONE => AONE(Nil)
  case CHAR(c) => ACHAR(Nil, c)
  case ALT(r1, r2) => AALT(Nil, fuse(List(Z), internalise(r1)), fuse(List(S), internalise(r2)))
  case SEQ(r1, r2) => ASEQ(Nil, internalise(r1), internalise(r2))
  case STAR(r) => ASTAR(Nil, internalise(r))
}


internalise(("a" | "ab") ~ ("b" | ""))



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
  case (STAR(r1), Z::bs) => {
    val (v, bs1) = decode_aux(r1, bs)
    val (Stars(vs), bs2) = decode_aux(STAR(r1), bs1)
    (Stars(v::vs), bs2)
  }
  case (STAR(_), S::bs) => (Stars(Nil), bs)
}

def decode(r: Rexp, bs: Bits) = decode_aux(r, bs) match {
  case (v, Nil) => v
  case _ => throw new Exception("Not decodable")
}

def encode(v: Val) : Bits = v match {
  case Empty => Nil
  case Chr(c) => Nil
  case Left(v) => Z :: encode(v)
  case Right(v) => S :: encode(v)
  case Sequ(v1, v2) => encode(v1) ::: encode(v2)
  case Stars(Nil) => List(S)
  case Stars(v::vs) =>  Z :: encode(v) ::: encode(Stars(vs))
}


// nullable function: tests whether the aregular 
// expression can recognise the empty string
def bnullable (r: ARexp) : Boolean = r match {
  case AZERO => false
  case AONE(_) => true
  case ACHAR(_,_) => false
  case AALTS(_, rs) => rs.exists(bnullable)
  case ASEQ(_, r1, r2) => bnullable(r1) && bnullable(r2)
  case ASTAR(_, _) => true
}

def bmkeps(r: ARexp) : Bits = r match {
  case AONE(bs) => bs
  case AALTS(bs, r::Nil) => bs ++ bmkeps(r) 
  case AALTS(bs, r::rs) => 
    if (bnullable(r)) bs ++ bmkeps(r) else bmkeps(AALTS(bs, rs))  
  case ASEQ(bs, r1, r2) => bs ++ bmkeps(r1) ++ bmkeps(r2)
  case ASTAR(bs, r) => bs ++ List(S)
}

// derivative of a regular expression w.r.t. a character
def bder(c: Char, r: ARexp) : ARexp = r match {
  case AZERO => AZERO
  case AONE(_) => AZERO
  case ACHAR(bs, d) => if (c == d) AONE(bs) else AZERO
  case AALTS(bs, rs) => AALTS(bs, rs.map(bder(c, _)))
  case ASEQ(bs, r1, r2) => 
    if (bnullable(r1)) AALT(bs, ASEQ(Nil, bder(c, r1), r2), fuse(bmkeps(r1), bder(c, r2)))
    else ASEQ(bs, bder(c, r1), r2)
  case ASTAR(bs, r) => ASEQ(bs, fuse(List(Z), bder(c, r)), ASTAR(Nil, r))
}

// derivative w.r.t. a string (iterates der)
@tailrec
def bders (s: List[Char], r: ARexp) : ARexp = s match {
  case Nil => r
  case c::s => bders(s, bder(c, r))
}

// main unsimplified lexing function (produces a value)
def blex(r: ARexp, s: List[Char]) : Bits = s match {
  case Nil => if (bnullable(r)) bmkeps(r) else throw new Exception("Not matched")
  case c::cs => blex(bder(c, r), cs)
}

def pre_blexing(r: ARexp, s: String)  : Bits = blex(r, s.toList)
def blexing(r: Rexp, s: String) : Val = decode(r, pre_blexing(internalise(r), s))

def flts(rs: List[ARexp]) : List[ARexp] = rs match {
  case Nil => Nil
  case AZERO :: rs => flts(rs)
  case AALTS(bs, rs1) :: rs => rs1.map(fuse(bs, _)) ++ flts(rs)
  case r1 :: rs => r1 :: flts(rs)
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


def bsimp(r: ARexp): ARexp = r match {
  case ASEQ(bs1, r1, r2) => (bsimp(r1), bsimp(r2)) match {
      case (AZERO, _) => AZERO
      case (_, AZERO) => AZERO
      case (AONE(bs2), r2s) => fuse(bs1 ++ bs2, r2s)
      //case (AALTS(bs, rs), r2) => AALTS(bs, rs.map(ASEQ(Nil, _, r2)))
      case (r1s, r2s) => ASEQ(bs1, r1s, r2s)
  }
  case AALTS(bs1, rs) => distinctBy(flts(rs.map(bsimp)), erase) match {
      case Nil => AZERO
      case r::Nil => fuse(bs1, r)
      case rs => AALTS(bs1, rs)
  }
  case r => r
}

val reg = ((("a" | ("a" | "b")) | ("b" | "c")) | ("e" | ("b" | "c")))

val reg11 = (("a"%) ~ ("b" | "c"))%

def blex_simp(r: ARexp, s: List[Char]) : Bits = s match {
  case Nil => if (bnullable(r)) bmkeps(r) 
              else throw new Exception("Not matched")
  case c::cs => blex(bsimp(bder(c, r)), cs)
}

def blexing_simp(r: Rexp, s: String) : Val = 
  decode(r, blex_simp(internalise(r), s.toList))



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


val evil1 = (("a").%) ~ "b"
val evil2 = (((("a").%).%).%) ~ "b"
val evil3 = (("a"~"a") | ("a")).%

for(i <- 1 to 10000 by 1000) {
    println(time_needed(1, blex_simp(internalise(evil1), ("a"*i + "b").toList)))
}

for(i <- 1 to 10000 by 1000) {
    println(time_needed(1, blexing_simp(evil1, "a"*i + "b")))
}

for(i <- 1 to 10000 by 1000) {
    println(time_needed(1, blexing_simp(evil2, "a"*i + "b")))
}

for(i <- 1 to 10000 by 1000) {
    println(time_needed(1, blexing_simp(evil3, "a"*i)))
}






/////////////////////////
/////////////////////////
///// Below not relevant


val rf = ("a" | "ab") ~ ("ab" | "")
println(pre_blexing(internalise(rf), "ab"))
println(blexing(rf, "ab"))
println(blexing_simp(rf, "ab"))

val r0 = ("a" | "ab") ~ ("b" | "")
println(pre_blexing(internalise(r0), "ab"))
println(blexing(r0, "ab"))
println(blexing_simp(r0, "ab"))

val r1 = ("a" | "ab") ~ ("bcd" | "cd")
println(blexing(r1, "abcd"))
println(blexing_simp(r1, "abcd"))

println(blexing((("" | "a") ~ ("ab" | "b")), "ab"))
println(blexing_simp((("" | "a") ~ ("ab" | "b")), "ab"))

println(blexing((("" | "a") ~ ("b" | "ab")), "ab"))
println(blexing_simp((("" | "a") ~ ("b" | "ab")), "ab"))

println(blexing((("" | "a") ~ ("c" | "ab")), "ab"))
println(blexing_simp((("" | "a") ~ ("c" | "ab")), "ab"))


// Sulzmann's tests
//==================

val sulzmann = ("a" | "b" | "ab").%

println(blexing(sulzmann, "a" * 10))
println(blexing_simp(sulzmann, "a" * 10))

for (i <- 0 to 4000 by 500) {
  println(i + ": " + "%.5f".format(time_needed(1, blexing_simp(sulzmann, "a" * i))))
}

for (i <- 0 to 15 by 5) {
  println(i + ": " + "%.5f".format(time_needed(1, blexing_simp(sulzmann, "ab" * i))))
}




// some automatic testing

def clear() = {
  print("")
  //print("\33[H\33[2J")
}

def merge[A](l1: LazyList[A], l2: LazyList[A], l3: LazyList[A]) : LazyList[A] =
  l1.head #:: l2.head #:: l3.head #:: merge(l1.tail, l2.tail, l3.tail)


// enumerates regular expressions until a certain depth
def enum(rs: LazyList[Rexp]) : LazyList[Rexp] = {
  rs #::: enum( (for (r1 <- rs; r2 <- rs) yield ALT(r1, r2)) ++ 
                (for (r1 <- rs; r2 <- rs) yield SEQ(r1, r2)) ++
                (for (r1 <- rs) yield STAR(r1)))
}


enum(LazyList(ZERO, ONE, CHAR('a'), CHAR('b'))).take(200).force.mkString("\n")
enum(LazyList(ZERO, ONE, CHAR('a'), CHAR('b'))).take(200_000).force



import scala.util.Try

def test_mkeps(r: Rexp) = {
  val res1 = Try(Some(mkeps(r))).getOrElse(None)
  val res2 = Try(Some(decode(r, bmkeps(internalise(r))))).getOrElse(None) 
  if (res1 != res2) println(s"Mkeps disagrees on ${r}")
  if (res1 != res2) Some(r) else (None)
}

println("Testing mkeps")
enum(LazyList(ZERO, ONE, CHAR('a'), CHAR('b'))).take(100).exists(test_mkeps(_).isDefined)
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
    val ad = bder('b', internalise(r))
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

def tests_bder(c: Char)(r: Rexp) = {
  val d = der(c, r)
  val vals = values(d)
  for (v <- vals) {
    println(s"Testing ${r} and ${v}")
    val res1 = retrieve(bder(c, internalise(r)), v)
    val res2 = encode(inj(r, c, decode(d, retrieve(internalise(der(c, r)), v))))
    if (res1 != res2) println(s"Disagree on ${r}, ${v} and der = ${d}")
    if (res1 != res2) println(s"   ${res1} !=  ${res2}")
    if (res1 != res2) Some((r, v)) else None
  }
}

println("Testing bder/der")
println(enum(2, "ab").map(tests_bder('a')).toSet)

val er = SEQ(ONE,CHAR('a')) 
val ev = Right(Empty) 
val ed = ALT(SEQ(ZERO,CHAR('a')),ONE)

retrieve(internalise(ed), ev) // => [true]
internalise(er)
bder('a', internalise(er))
retrieve(bder('a', internalise(er)), ev) // => []
decode(ed, List(true)) // gives the value for derivative
decode(er, List())     // gives the value for original value


val dr = STAR(CHAR('a'))
val dr_der = SEQ(ONE,STAR(CHAR('a'))) // derivative of dr
val dr_val = Sequ(Empty,Stars(List())) // value of dr_def


val res1 = retrieve(internalise(der('a', dr)), dr_val) // => [true]
val res2 = retrieve(bder('a', internalise(dr)), dr_val) // => [false, true]
decode(dr_der, res1) // gives the value for derivative
decode(dr, res2)     // gives the value for original value

encode(inj(dr, 'a', decode(dr_der, res1)))

