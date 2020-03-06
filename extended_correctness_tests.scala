
def getString(r: Rexp, bs: Bits) = flatten(decode(r, bs))

def test(r: Rexp, s: String) : Boolean = {
    val bs = lexer(r, s)
    s == getString(r, bs)
}

def test_negative(r: Rexp, s: String) :  Boolean = {
    try{
        lexer(r, s)
        false
    }
    catch{
        case _ : Throwable => true
    }
}

def testList(r: Rexp, ls: List[String]) : Boolean = ls match {
    case Nil => true
    case head::tail => if(test(r, head)) testList(r, tail) 
                            else throw new Exception("Failed for: " + head)
}

def testList_negative(r: Rexp, ls: List[String]) : Boolean = ls match {
    case Nil => true
    case head::tail => if(test_negative(r, head)) testList_negative(r, tail) 
                            else throw new Exception("Failed for: " + head)
}

///tests

//(a{1,2} + b){2,..}
val reg0 = ("a"%(1, 2) | "b")>2
//positive tests
testList(reg0, List(
    "aa",
    "bb",
    "aab",
    "ab",
    "aaaaa",
    "bbbbbb",
    "aabbaabbaabababab"*500
))
test(reg0, "bb")
test(reg0, "aaaa")
test(reg0, "aaaaaaaa")
test(reg0, "aaaaaaaaaaaaaa")
test(reg0, "aaaaaaaaaaaaaaaaaaaa")
test(reg0,  "aabbaabbaabababab"*70)
lexer(reg0, "aaaaaaaaaaaaaaaaaaaaaaaaaaaa")
//negative tests
test_negative(reg0, "c")
test_negative(reg0, "aaaaabbaabbc")


val reg1 = ("a"|"e")%
test(reg1, "")
test(reg1, "a")
test(reg1, "e")
test(reg1, "a"*100)
test(reg1, "e"*100)
test(reg1, "aeaeaeeeeaaaaaaaaaa"*100)

val reg2 = (("a"%(2, 4)) ~ ("c" | "d"))%
test(reg2, "")
test(reg2, "aac")
test(reg2, "aad")
test(reg2,  "aaac")
test(reg2, "aaad")
test(reg2, "aaaac")
test(reg2, "aaaad")

test_negative(reg2, "aaaaac")
test_negative(reg2, "aacd")

val reg3 = (((("a")?)%(2,3))~("b"))%
test(reg3, "")
test(reg3, "b")
test(reg3, "aab")
test(reg3, "aaabaabbbbbaaab")
test(reg3, "aaabab")

test_negative(reg3, "aaaabab")
test_negative(reg3, "aaaba")

val reg4 = (((("ab")%(1,2))?) ~ ("c" | "d"))%
test(reg4, "")
test(reg4, "c")
test(reg4, "d")
test(reg4, "dcddcdcdccc")
test(reg4, "cababd")
test(reg4, "abc")

val reg5 = (((("a")%(1,2))?) ~ ("b" | "c"))%
test(reg5, "")
test(reg5, "c")
test(reg5, "ac")

val reg6 = ((("a"%(1, 2)) | "x") ~ ("b" | "c"))%
test(reg6, "aab")

val reg7 = ((("a"%) | "b") ~ ("c" | "d"))%
test(reg7, "acbdac")

flatten(decode(reg7, lexer(reg7, "ac")))
val reg8 = ("a" | (("b"%)~"e") | "c" | "d")%
test(reg8, "be")
test(reg8, "beabbbec")


erase(simp(internalise(reg8)))

val reg9 = ((("a"%)~"b") | "c")%
test(reg9, "abbbaabc")

val reg10 = "a"%(2, 4)
test(reg10, "aaa")

val reg4 = ((("ab")?)%(1,2))%
val reg5 = (((("ab")?)%(1,2)) ~ "c")%
val reg6 = ((("a")?) ~ ("b" | "c"))
val reg7 = (("a"%(1, 2)) | ("b"%(1, 2)))%
val reg8 = ("a"|"c")%
val reg9 = (("a") ~ ("b" | "c"))%
val reg10 = (("a"?) ~ ("b" | "c"))%
val reg11 = (("a"%) ~ ("b" | "c"))%
val reg12 = (("a"<2) ~ "b")%

val reg13 = "a"!
test(reg13, "b")

val reg14 = ("a"!)%
test(reg14, "twhjjfwui")

val reg15 = (((NOT(RANGE(('a' to 'z').toSet)))%) | "a")%
test(reg15, "1")
test(reg15, "1231412412&")
test(reg15, "a1231a")

test_negative(reg15, "abv3")

val reg16 = NOT("ab")
test(reg16, "ab")

val reg17 = NOT("a" | "b")
test(reg17, "e")

val reg18 = "a" | "b"
test(reg18, "b")

