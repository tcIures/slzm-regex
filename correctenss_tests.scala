def test(r: Rexp, s: String) : Unit = {
    val bs = lexer(r, s)
    assert(s == flatten(decode(r, bs)))
}

def test_negative(r: Rexp, s: String) : Boolean = {
    try {
        val bs = lexer(r, s)
        assert(s != flatten(decode(r, bs)))
        false
    }catch {
        case _ => true
    }
   
}


implicit def charlist2rexp(s: List[Char]): Rexp = s match {
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

//(a*(b+c))*
val reg1 = (("a"%) ~ ("b" | "c"))%

//positive tests
test(reg1, "")
test(reg1, "b")
test(reg1, "c")
test(reg1, "bbbb")
test(reg1, "cccc")
test(reg1, "bbbbbccccc")
test(reg1, "bcbcbcbcbcbcb")
test(reg1, "ab")
test(reg1, "ac")
test(reg1, "abbb")
test(reg1, "acccc")
test(reg1, "abbbbccc")
test(reg1, "accccbbbb")
test(reg1, "abcbcbcbcbc")
test(reg1, "abaaaabbbaaaaacccbbbbaabcbcbcbaaaaab")

//negative tests
test_negative(reg1, "e")
test_negative(reg1, "a")
test_negative(reg1, "aba")
test_negative(reg1, "aca")
test_negative(reg1, "a"*20+"bc"*30+"a"*20+"bca")

//((a+b)c*)*
val reg2 = (("a" | "b") ~ (("c")%))%

//positive tests
test(reg2, "")
test(reg2, "a")
test(reg2, "b")
test(reg2, "aaa")
test(reg2, "bbb")
test(reg2, "aaaabbb")
test(reg2, "bbbaaaaa")
testt(reg2, "abababababab")
test(reg2, "ac")
test(reg2, "bc")
test(reg2, "aaac")
test(reg2, "bbbc")
test(reg2, "aaabbbc")
test(reg2, "bbbaaac")
test(reg2, "abababababc")
test(reg2, "ababcababccccccccbccccccccaaaabbbbababbccccccccb")


//((a+b)(c+d))*
val reg3 = (("a" | "b") ~ ("c" | "d")) %

//positive tests
test(reg3, "")
test(reg3, "ac")
test(reg3, "ad")
test(reg3, "bc")
test(reg3, "bd")
test(reg3, "ac"*10)
test(reg3, "ad"*10)
test(reg3, "bc"*10)
test(reg3, "bd"*10)
test(reg3, "acad"*10)
test(reg3, "adac"*10)
test(reg3, "bcbd"*10)
test(reg3, "bdbc"*10)

//((a+b)*d*)*e*

val reg4 = (((("a" | "b")%) ~ ("d"%))%) ~ ("e"%)
test(reg4, "")
test(reg4, "a")
test(reg4, "b")
test(reg4, "d")
test(reg4, "e")
test(reg4, "aabddbdbadee")
test(reg4, "dabdadbadbadbadbadbadbadbdbdbeee")
test(reg4, "eeeee")
test(reg4, "a"*20)
test(reg4, "b"*20)
test(reg4, "d"*20)
test(reg4, "e"*20)
test(reg4, "aaabbbdddee")
test(reg4, "aaae")
test(reg4, "bbbe")
test(reg4, "ddde")

val reg5 = (("a" | "b" | "c" | "d")%) 

flatten(decode(reg5, lexer(reg5, "abcdaaabbccdd")))

val reg6 = ((("a" | "b" | "c" | "d")%) ~ (("1" | "2" | "3" | "4")%))%

flatten(decode(reg5, lexer(reg6, "aa1")))


val evil4 = ((("a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | 
                        "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y")%) ~ (("1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" | "0")%))%
val evil5 = (evil4%) ~ "@" ~ (evil4) ~ ".com"