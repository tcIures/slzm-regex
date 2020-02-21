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

//negative tests
test_negative(reg2, "d")
test_negative(reg2, "cccccccccccc")
test_negative(reg2, "acccccccccccc"*50 + "d")


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

//negative tests
test_negative(reg3, "add")
test_negative(reg3, "bcd")
test_negative(reg3, "abc")

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
test(reg5, "")
test(reg5, "a")
test(reg5, "b")
test(reg5, "c")
test(reg5, "d")
test(reg5, "aaaa")
test(reg5, "bbbb")
test(reg5, "cccc")
test(reg5, "ddddd")
test(reg5, "abcdaaabbccdd")

val reg6 = ((("a" | "b")%) ~ (("1" | "2" )%))%
test(reg6, "")
test(reg6, "a")
test(reg6, "b")
test(reg6, "1")
test(reg6, "2")
test(reg6, "a1")
test(reg6, "a2")
test(reg6, "b1")
test(reg6, "b2")
test(reg6, "a1b1")
test(reg6, "aaaa1")
test(reg6, "abaaab12121212ab12b12bbb")
test(reg6, "1212121212aaa")
test(reg6, "aaaa")
test(reg6, "bbbb")
test(reg6, "1111")

val reg6 = ((("a" | "b" | "c" | "d")%) ~ (("1" | "2" | "3" | "4")%))%
test(reg6, "")
test(reg6, "a")
test(reg6, "abcdddcddd")
test(reg6, "a11")
test(reg6, "aa11bbcc2233")


val reg7 = ((("a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | 
                        "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | ".")%) ~ 
                            (("1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" | "0")%))%
val reg8 = (reg7%) ~ "@" ~ (reg7) ~ ".com"

test(reg8, "tudor12@gmail.com")