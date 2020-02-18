def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  (end - start)/(i * 1.0e9)
}


val evil1 = (("a")%) ~ "b"
val evil2 = (((("a")%)%)%) ~ "b"
val evil3 = (("a"~"a") | ("a"))%
val evil4 = ((("a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | 
                        "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y")%) ~ (("1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" | "0")%))%
val evil5 = (evil4%) ~ "@" ~ (evil4) ~ ".com"


for(i <- 1 to 10000 by 1000) {
    println(time_needed(1, lexer(evil1, "a"*i + "b")))
}

for(i <- 1 to 10000 by 1000) {
    println(time_needed(1, lexer(evil2, "a"*i + "b")))
}

for(i <- 1 to 10000 by 1000) {
    println(time_needed(1, lexer(evil3, "a"*i)))
}




