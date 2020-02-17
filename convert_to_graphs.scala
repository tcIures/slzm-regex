import scala.language.implicitConversions    
import scala.language.reflectiveCalls
import scala.annotation.tailrec   
import sys.process._

/*abstract class Rexp 
case object ZERO extends Rexp
case object ONE extends Rexp
case class CHAR(c: Char) extends Rexp
case class ALT(r1: Rexp, r2: Rexp) extends Rexp 
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp 
case class STAR(r: Rexp) extends Rexp 

abstract class Bit
case object S extends Bit
case object Z extends Bit

type Bits = List[Bit]

implicit def bit2bits(b: Bit) : Bits = List(b)

abstract class ARexp
case object AZERO extends ARexp
case class AONE(bs: Bits) extends ARexp
case class ACHAR(bs: Bits, c: Char) extends ARexp
case class AALTs(bs: Bits, l: List[ARexp]) extends ARexp
case class ASEQ(bs: Bits, r1: ARexp, r2: ARexp) extends ARexp
case class ASTAR(bs: Bits, r: ARexp) extends ARexp*/

abstract class GARexps
case class GAZEROs(index : Int) extends GARexps
case class GAONEs(index: Int, bs: Bits) extends GARexps
case class GACHARs(index: Int, bs: Bits, c: Char) extends GARexps
case class GAALTs(index: Int, bs: Bits, l: List[GARexps]) extends GARexps
case class GASEQs(index: Int, bs: Bits, r1: GARexps, r2: GARexps) extends GARexps
case class GASTARs(index: Int, bs: Bits, r: GARexps) extends GARexps

/*def fuse(bs: Bits, r: ARexp) : ARexp = r match {
    case AZERO => AZERO
    case AONE(bsq) => AONE(bs ++ bsq)
    case ACHAR(bsq, c) => ACHAR(bs ++ bsq, c)
    case AALTs(bsq, ls) => AALTs(bs ++ bsq, ls)
    case ASEQ(bsq, r1, r2) => ASEQ(bs ++ bsq, r1, r2)
    case ASTAR(bsq, r) => ASTAR(bs ++ bsq, r)
}

def internalise(r: Rexp) : ARexp = r match {
    case ZERO => AZERO
    case ONE => AONE(List())
    case CHAR(c) => ACHAR(List(), c)
    case ALT(r1, r2) => AALTs(List(), List(fuse(Z, internalise(r1)), 
                                                    fuse(S, internalise(r2))))
    case SEQ(r1, r2) => ASEQ(List(), internalise(r1), internalise(r2))
    case STAR(r) => ASTAR(List(), internalise(r))
}*/


   
// some convenience for typing in regular expressions
/*def charlist2rexp(s : List[Char]): Rexp = s match {
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
}*/

def bsToString(ls: Bits) : String = ls match {
    case Nil => ""
    case head::tail => if(head == S) "1" + bsToString(tail)
                        else "0" + bsToString(tail)
}

def regToString(r: ARexp) : String = r match {
    case AZERO => "0"
    case AONE(bs) => "1 : " + bsToString(bs)
    case ACHAR(bs, c) => c + " : " + bsToString(bs)
    case AALTs(bs, _) => "+ : " + bsToString(bs)
    case ASEQ(bs, _, _) => "~ : " + bsToString(bs)
    case ASTAR(bs, _)  => "* : " + bsToString(bs)
}

def node(r: ARexp, index: Int) : String = {
    "dot.node(\'" + index + "\', \'" + regToString(r) + "\')"  
}

def edge(index1: Int, index2: Int) : String = {
    "dot.edge(\'" + index1 + "\', \'" + index2 + "\')"
}

var index = -1

def g_internalise(r: ARexp) : GARexps = {index+=1; r match {
    case AZERO => GAZEROs(index)
    case AONE(bs) => GAONEs(index, bs)
    case ACHAR(bs, c) => GACHARs(index, bs, c)
    case AALTs(bs, ls) => GAALTs(index, bs, ls.map(g_internalise(_)))
    case ASEQ(bs, r1, r2) => GASEQs(index, bs, g_internalise(r1), g_internalise(r2))
    case ASTAR(bs, r) => GASTARs(index, bs, g_internalise(r))
}}

def getGraph(r: ARexp) : GARexps = {index = -1; g_internalise(r)}

def getNodesCode(g: GARexps) : String = g match{
    case GAZEROs(index) => node(AZERO, index) + "\n"
    case GAONEs(index, bs) => node(AONE(bs), index) + "\n"
    case GACHARs(index, bs, c) => node(ACHAR(bs, c), index) + "\n"
    case GAALTs(index, bs, ls) => node(AALTs(bs, List()), index) + "\n" + ls.map(getNodesCode(_)).mkString + "\n"
    case GASEQs(index, bs, r1, r2) => node(ASEQ(bs, AZERO, AZERO), index) + "\n" + getNodesCode(r1) + "\n" +  getNodesCode(r2) + "\n"
    case GASTARs(index, bs, r) => node(ASTAR(bs, AZERO), index) + "\n" + getNodesCode(r) + "\n"
}

def getIndex(g: GARexps) : Int = g match {
    case GAZEROs(index) => index
    case GAONEs(index, _) => index
    case GACHARs(index, _, _) => index
    case GAALTs(index, _, _) => index
    case GASEQs(index, _, _, _) => index
    case GASTARs(index, _, _) => index
}

def getEdgeCode(g: GARexps, parent: Int) : String = {
    val edgeString = if(getIndex(g) != 0) edge(parent, getIndex(g)) else ""
    val str = g match {
        case GAZEROs(index) => "\n"
        case GAONEs(index, _) => "\n"
        case GACHARs(index, _, _) => "\n"
        case GAALTs(index, _, ls) =>  "\n" + ls.map(getEdgeCode(_, index)).mkString + "\n"
        case GASEQs(index, _, g1, g2) => "\n" + getEdgeCode(g1, index) + "\n" + getEdgeCode(g2, index) + "\n"
        case GASTARs(index, _, g1) => "\n" + getEdgeCode(g1, index) + "\n"
    }
    edgeString + str
}

def getPythonCode(g: GARexps) : String = getNodesCode(g) + "\n" + getEdgeCode(g, 0) + "\n"

def writeCode(r: ARexp, i: Int) = {
    val x0 = "from graphviz import Digraph" + "\n" + "dot = Digraph(comment='Iteration: " + i + "')" + "\n"

    val x1 = getPythonCode(getGraph(r))

    val x2 = "dot.render(\'left-multiply-bug/it" + i + ".gv\', view=True)"

    val string = x0+x1+x2

    // PrintWriter
    import java.io._
    val pw = new PrintWriter(new File("code.py" ))
    pw.write(string)
    pw.close
}

val regex = ((("a")%) ~ ("b" | "c"))%


val reg = internalise(regex)

val it0 = simp(reg)
val it1 = der(it0, 'a')
val it10 = simp(it1)
val it2 = der(it10, 'b')
val it20 = simp(it2)
val it200 =simp(it20)

writeCode(it0, 0)
writeCode(it1, 1)
writeCode(it10, 10)
writeCode(it2, 2)
writeCode(it20, 20)
writeCode(it200, 200)




/*def getGraphPdf(r: Rexp, s: String, index: Int) : Unit = s.toList match {
    case Nil => Nil
    case head::tail => {
        val it1 = der(r, head)
        writeCode(it1, index)
        val re1 = "python3 code.py" !
        val it2 = simp(it1)
        writeCode(it2, index+1)
        val re2  = "python3 code.py" !
        val x = getGraphPdf(it2, tail.mkString, index+2)
        Nil
        
    }
}

def getPdf(r: Rexp, s: String, dir: String) = {
    val start = simp(internalise(r))
    writeCode(start, 0)
    val re = "python3 code.py" !
    val result = getGraphPdf(r, s, 1)
    //val result1 = ("cd " + dir) !
    //val result2 = "find . -type f ! -iname \"*.pdf\" -delete"!
} 

getGraphPdf(regex, "aab", 0)*/
