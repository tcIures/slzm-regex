class FixedList[A](max: Int) extends Iterable[A] {

  val list: ListBuffer[A] = ListBuffer()

  def add (elem: A) : Unit = {
    if (list.size < max) {
      list.append(elem)
    }
  }

  override def foreach[U](f: A => U) = list.foreach(f)

  override def iterator = list.iterator

}

def size(r: ARexp) : Int = r match {
    case AZERO => 1
    case AONE(_) => 1
    case ACHAR(_, _) => 1
    case AALTs(_, ls) => 1 + ls.map(size(_)).sum
    case ASEQ(_, r1, r2) => 1 + size(r1) + size(r2)
    case AFROM(_, r, _) => 1 + size(r)
    case ABETWEEN(_, r, _, _) => 1 + size(r)
}


def lex_info(r: ARexp, s: List[Char], ls: FixedList[Int]) : FixedList[Int] = s match {
    case Nil => {
        if(nullable(r)) {
            ls.add(size(r)) 
            ls
        }else throw new Exception("Not matched")
    }
    case c::cs => {
        var newLs = lex_info(simp(der(r, c)), cs, ls)
        newLs.add(size(r))
        newLs
    }
}

def lexer_info(r: Rexp, s: String, n: Int) : FixedList[Int] = 
                                lex_info((internalise(erase(normalize(internalise(r))))), s.toList,
                                     new FixedList[Int](n))



