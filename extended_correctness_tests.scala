
def getString(r: Rexp, bs: Bits) = flatten(decode(r, bs))

def test(r: Rexp, s: String) = {
    val bs = lexer(r, s)
    assert(s == getString(r, bs))
}

///tests

//(a{1,2} + x){2,..}
val reg0 = ("a"%(1, 2) | "x")>2
test(reg0, "aa")
test(reg0, "xx")
test(reg0, "aax")
test(reg0, "aaaaaaaxxxaaaa")

val reg1 = ("a"|"e")%
test(reg1, "")
test(reg1, "a")
test(reg1, "e")
test(reg1, "aaee")

val reg2 = (("a"%(2, 4)) ~ ("c" | "d"))%
test(reg2, "")
test(reg2, "aac")
test(reg2, "aad")
test(reg2,  "aaac")
test(reg2, "aaad")
test(reg2, "aaaac")
test(reg2, "aaaad")

val reg3 = (((("a")?)%(2,3))~("b"))%
test(reg3, "")
test(reg3, "b")
test(reg3, "aab")
test(reg3, "aaabaabbbbbaaab")

val reg4 = (((("ab")%(1,2))?) ~ ("c" | "d"))%
test(reg4, "")
test(reg4, "abc")


val reg4 = ((("ab")?)%(1,2))%
val reg5 = (((("ab")?)%(1,2)) ~ "c")%
val reg6 = ((("a")?) ~ ("b" | "c"))
val reg7 = (("a"%(1, 2)) | ("b"%(1, 2)))%
val reg8 = ("a"|"c")%
val reg9 = (("a") ~ ("b" | "c"))%
val reg10 = (("a"?) ~ ("b" | "c"))%
val reg11 = (("a"%) ~ ("b" | "c"))%
val reg12 = (("a"<2) ~ "b")%