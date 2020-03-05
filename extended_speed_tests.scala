import scala.language.implicitConversions
import scala.language.reflectiveCalls
import scala.language.postfixOps 
import scala.util.matching.Regex

def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  (end - start)/(i * 1.0e9)
}

def scalaMatch(r: Regex, s: String) = r.findFirstIn(s).getOrElse("error")
def sulzmanMatch(r: Rexp, s: String) : String = {
    try {
        lexer(r, s)
        "succes"
    }catch {
        case _:Throwable => "error"
    }
}

def testString(n: Int) : String = "a" * n

def maxValueScala(r: Regex, n: Int) : Int = {
  try {
      scalaMatch(r, testString(n))
      maxValueScala(r, n + 100)
  }
  catch {
    case _ : Throwable => n
  }
}

def maxValue(r: Rexp, n: Int) : Int = {
    try {
      sulzmanMatch(r, testString(n))
      maxValue(r, n + 100)
  }
  catch {
    case _ : Throwable => n
  }
}

//(a?){n}a{n}
def evil0(n: Int) : Rexp = (("a"?)%(n)) ~ (("b")%(n))
def scalaEvil0(n: Int) : Regex = (s"^(a?){$n}b{$n}$$").r

for(i <- 1 to 5000 by 500) {
    println(i + ": " + time_needed(1, lexer(evil0(i), "a"*i + "b"*i)))
}


for(i <- 1 to 3000 by 500) {
    println(i + ": " + time_needed(1, lexer(evil0(i), "a"*i + "b"*i)) + "; " + 
        time_needed(1, scalaMatch(scalaEvil0(i), "a"*i + "b"*i)))
}


//(a?){n}a{n}
def evil1(n: Int) : Rexp = (("a"?)%(n)) ~ (("a")%(n))

def scalaEvil1(n: Int) : Regex = (s"^(a?){$n}a{$n}$$").r


for(i <- 0 to 1000 by 200) {
    println(i + ": " + time_needed(1, lexer(evil1(i), "a"*i + "a"*i)) + "; " + 
        time_needed(1, scalaMatch(scalaEvil1(i), "a"*i + "a"*i)))
}

val evil2 = ((("a" | ('b' to 'z').toList)$))%
val scalaEvil2 = "([a-z]+)*".r

for(i <- 0 to 8000 by 1000) {
    println(i + ": " + time_needed(1, sulzmanMatch(evil2, "a"*i+"!")) + "; " + 
        time_needed(1, scalaMatch(scalaEvil2, "a"*i+"!")))
}

val evil3 = ("a" | "aa")%
val scalaEvil3 = "(a|aa)*".r

for(i <- 500 to 3000 by 500) {
    println(i + ": " + time_needed(1, sulzmanMatch(evil3, "a"*i+"b")))
}

for(i <- 500 to 3000 by 500) {
    println(i + ": " + time_needed(1, sulzmanMatch(evil3, "a"*i+"b")) + "; " + 
        time_needed(1, scalaMatch(scalaEvil3, "a"*i+"b")))
}

val evil4 = ((("a")%)%) ~ "b"
val scalaEvil4 = "(a*)*b".r

for(i <- 500 to 5000 by 500) {
    println(i + ": " + time_needed(1, sulzmanMatch(evil4, "a"*i)))
}

for(i <- 500 to 5000 by 500) {
    println(i + ": " + time_needed(1, sulzmanMatch(evil4, "a"*i)) + "; " + 
        time_needed(1, scalaMatch(scalaEvil4, "a"*i)))
}

val evil5 = ("a" | ("a"?))%
val scalaEvil5 = "(a|(a?))*".r

for(i <- 0 to 5000 by 500) {
    println(i + ": " + time_needed(10, sulzmanMatch(evil5, "a"*i)))
}

for(i <- 0 to 7500 by 250) {
    println(i + ": " + time_needed(1, sulzmanMatch(evil5, "a"*i)) + "; " + 
        time_needed(1, scalaMatch(scalaEvil5, "a"*i)))
}

val evil6 = (("a")?)%

for(i <- 0 to 7500 by 250) {
    println(i + ": " + time_needed(10, sulzmanMatch(evil6, "a"*i)))
}

val evil7 = ("a" | "aa" | "aaa" | "aaaa")%

for(i <- 0 to 7500 by 250) {
    println(i + ": " + time_needed(10, sulzmanMatch(evil7, "a"*i)))
}

val reg1 = ((("a" | ('b' to 'z').toList)$) ~ ":" ~ (("1" | ('2' to '9').toList)$))%
val scalaReg1 = "([a-z]+:[1-9]+)*".r


for(i <- 0 to 1750 by 200) {
    println(i + ": " + time_needed(1, sulzmanMatch(reg1, "abc:12"*i)) + "; " + 
        time_needed(1, scalaMatch(scalaReg1, "abc:12"*i)))
}

val reg2 = ("a"%(1, 2) | "b")>2
val scalaReg2 = "(a{1,2}|b){2,}".r

for(i <- 0 to 5000 by 500) {
    println(i + ": " + time_needed(1, sulzmanMatch(reg2, "a"*i)) + "; " + 
        time_needed(1, scalaMatch(scalaReg2, "a"*i)))
}

val reg3 = ("a" | "b")>2
val scalaReg3 = "(a|b){2,}".r

for(i <- 0 to 3000 by 500) {
    println(i + ": " + time_needed(1, sulzmanMatch(reg3, "a"*i)) + "; " + 
        time_needed(1, scalaMatch(scalaReg3, "a"*i)))
}

val reg4 = ("a"%(2, 3) | "b")>2
val scalaReg4 = "(a{1,2}|b){2,}".r

for(i <- 0 to 3000 by 500) {
    println(i + ": " + time_needed(5, sulzmanMatch(reg4, "a"*i)) + "; " + 
        time_needed(5, scalaMatch(scalaReg4, "a"*i)))
}

val reg5 = ("aa" | "aaa" | "b")>2
val scalaReg5 = "(aa|aaa|b){2,}".r

for(i <- 0 to 3000 by 500) {
    println(i + ": " + time_needed(1, sulzmanMatch(reg5, "a"*i)) + "; " + 
        time_needed(1, scalaMatch(scalaReg5, "a"*i)))
}

val reg6 = ((("a")$)$) ~ "b"
val scalaReg6 = "(a+)+b".r

for(i <- 0 to 3000 by 500) {
    println(i + ": " + time_needed(1, sulzmanMatch(reg6, "a"*i)) + "; " + 
        time_needed(1, scalaMatch(scalaReg6, "a"*i)))
}

val reg7 = "ab" ~ ((NOT("="))$) ~ "="
val scalaReg7 = "ab[^=]+=".r

for(i <- 0 to 5000 by 500) {
    println(i + ": " + time_needed(1, sulzmanMatch(reg7, "ab"*i+"=")) + "; " + 
        time_needed(1, scalaMatch(scalaReg7, "ab"*i+"=")))
}
val scalaReg8 = "(([\\-.]|[_]+)?([a-zA-Z0-9]+))*".r

val lwr = RANGE(('a' to 'z').toSet)
val upr = RANGE(('a' to 'z').toSet)
val nrs = RANGE(('a' to 'z').toSet)

val reg9 = ((((lwr)$) ~ ((lwr | nrs)$)$) ~ upr) ~ ((lwr)$)
val scalaReg9 = "^(([a-z])+[a-z0-9])+[A-Z]([a-z])+$".r

for(i <- 0 to 100000 by 5000) {
    println(i + ": " + time_needed(1, sulzmanMatch(reg9, "a"*i+"!")) + "; " + 
        time_needed(1, scalaMatch(scalaReg9, "a"*i+"!")))
}

for(i <- 0 to 10000000 by 100000) {
    println(i + ": " + time_needed(1, sulzmanMatch(reg9, "a"*i+"!")))
}

val reg10 = (lwr)%
val scalaReg10 = "[a-z]*".r

for(i <- 0 to 100000 by 10000) {
    println(i + ": " + time_needed(1, sulzmanMatch(reg10, "a"*i+"!")))
}

for(i <- 0 to 100000 by 10000) {
    println(i + ": " + time_needed(1, sulzmanMatch(reg10, "z"*i+"!")))
}


for(i <- 0 to 100000 by 10000) {
    println(i + ": " + time_needed(1, sulzmanMatch(reg10, "a"*i+"!")) + "; " + 
        time_needed(1, scalaMatch(scalaReg10, "a"*i+"!")))
}

for(i <- 0 to 100000 by 10000) {
    println(i + ": " + time_needed(1, sulzmanMatch(reg10, "n"*i+"!")))
}


for(i <- 0 to 100000 by 10000) {
    println(i + ": " + time_needed(1, sulzmanMatch(reg10, "z"*i+"!")) + "; " + 
        time_needed(1, scalaMatch(scalaReg10, "z"*i+"!")))
}

/// [a-z] as ALT(a, ALT(b, ...))

implicit def chalist2Alt(ls: List[Char]) : Rexp = ls match {
    case Nil => ONE
    case c::Nil => CHAR(c)
    case c::s => ALT(CHAR(c), chalist2Alt(s))
}

val lwr = ("a" | ('b' to 'z').toList)
val upr = ("a" | ('b' to 'z').toList)
val nrs = ("a" | ('b' to 'z').toList)


val reg9 = ((((lwr)$) ~ ((lwr | nrs)$)$) ~ upr) ~ ((lwr)$)
val scalaReg9 = "^(([a-z])+[a-z0-9])+[A-Z]([a-z])+$".r

for(i <- 0 to 100000 by 10000) {
    println(i + ": " + time_needed(1, sulzmanMatch(reg9, "a"*i+"!")))
}

for(i <- 0 to 100000 by 10000) {
    println(i + ": " + time_needed(1, sulzmanMatch(reg9, "z"*i+"!")))
}

val reg10 = (lwr)%


for(i <- 0 to 100000 by 10000) {
    println(i + ": " + time_needed(1, sulzmanMatch(reg10, "a"*i+"!")))
}

for(i <- 0 to 100000 by 10000) {
    println(i + ": " + time_needed(1, sulzmanMatch(reg10, "n"*i+"!")))
}

for(i <- 0 to 100000 by 10000) {
    println(i + ": " + time_needed(1, sulzmanMatch(reg10, "z"*i+"!")))
}



