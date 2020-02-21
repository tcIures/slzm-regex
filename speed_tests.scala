def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  (end - start)/(i * 1.0e9)
}


val evil1 = (("a")%) ~ "b"
val evil2 = (((("a")%)%)%) ~ "b"
val evil3 = (("a"~"a") | ("a"))%





for(i <- 1 to 10000 by 1000) {
    println(time_needed(1, lexer(evil1, "a"*i + "b")))
}

for(i <- 1 to 10000 by 1000) {
    println(time_needed(1, lexer(evil2, "a"*i + "b")))
}

for(i <- 1 to 10000 by 1000) {
    println(time_needed(1, lexer(evil3, "a"*i)))
}




