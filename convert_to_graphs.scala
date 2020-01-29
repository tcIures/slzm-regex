import scala.language.implicitConversions    
import scala.language.reflectiveCalls
import scala.annotation.tailrec   

abstract class Rexp 
case object ZERO extends Rexp
case object ONE extends Rexp
case class CHAR(c: Char) extends Rexp
case class ALT(r1: Rexp, r2: Rexp) extends Rexp 
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp 
case class STAR(r: Rexp) extends Rexp 

abstract class ARexps
case object AZEROs extends ARexps
case class AONEs(bs: List[Boolean]) extends ARexps
case class ACHARs(bs: List[Boolean], c: Char) extends ARexps
case class AALTs(bs: List[Boolean], l: List[ARexps]) extends ARexps
case class ASEQs(bs: List[Boolean], r1: ARexps, r2: ARexps) extends ARexps
case class ASTARs(bs: List[Boolean], r: ARexps) extends ARexps

abstract class GARexps
case class GAZEROs(index : Int) extends GARexps
case class GAONEs(index: Int, bs: List[Boolean]) extends GARexps
case class GACHARs(index: Int, bs: List[Boolean], c: Char) extends GARexps
case class GAALTs(index: Int, bs: List[Boolean], l: List[GARexps]) extends GARexps
case class GASEQs(index: Int, bs: List[Boolean], r1: GARexps, r2: GARexps) extends GARexps
case class GASTARs(index: Int, bs: List[Boolean], r: GARexps) extends GARexps

def fuse(bs: List[Boolean], r: ARexps) : ARexps = r match {
    case AZEROs => AZEROs
    case AONEs(bsq) => AONEs(bs ++ bsq)
    case ACHARs(bsq, c) => ACHARs(bs ++ bsq, c)
    case AALTs(bsq, ls) => AALTs(bs ++ bsq, ls)
    case ASEQs(bsq, r1, r2) => ASEQs(bs ++ bsq, r1, r2)
    case ASTARs(bsq, r) => ASTARs(bs ++ bsq, r)
}

def getLs(r: ARexps): List[ARexps] = r match {
    case AALTs(bs, ls) => flatten_alt(ls.map(e => fuse(bs, e)))
    case r => List(r)
}

def flatten_alt(ls: List[ARexps]) : List[ARexps] = ls match {
    case Nil => List()
    case head::Nil => getLs(head)
    case head::tail => getLs(head) ++ flatten_alt(tail)
}

def internalise(r: Rexp) : ARexps = r match {
    case ZERO => AZEROs
    case ONE => AONEs(List())
    case CHAR(c) => ACHARs(List(), c)
    case ALT(r1, r2) => AALTs(List(), List(fuse(List(false), internalise(r1)), 
                                                    fuse(List(true), internalise(r2))))
    case SEQ(r1, r2) => ASEQs(List(), internalise(r1), internalise(r2))
    case STAR(r) => ASTARs(List(), internalise(r))
}


   
// some convenience for typing in regular expressions
def charlist2rexp(s : List[Char]): Rexp = s match {
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
}

val test = (("a"~"b") | ("c"))% 

val test1 = fuse(List(true, true, false), internalise(test))

def bsToString(ls: List[Boolean]) : String = ls match {
    case Nil => ""
    case head::tail => if(head) "1" + bsToString(tail)
                        else "0" + bsToString(tail)
}

def regToString(r: ARexps) : String = r match {
    case AZEROs => "0"
    case AONEs(bs) => "1 : " + bsToString(bs)
    case ACHARs(bs, c) => c + " : " + bsToString(bs)
    case AALTs(bs, _) => "+ : " + bsToString(bs)
    case ASEQs(bs, _, _) => "~ : " + bsToString(bs)
    case ASTARs(bs, _)  => "* : " + bsToString(bs)
}

def node(r: ARexps, index: Int) : String = {
    "dot.node(\'" + index + "\', \'" + regToString(r) + "\')"  
}

def edge(index1: Int, index2: Int) : String = {
    "dot.edge(\'" + index1 + "\', \'" + index2 + "\')"
}

def g_internalise(r: ARexps) : GARexps = {index+=1; r match {
    case AZEROs => GAZEROs(index)
    case AONEs(bs) => GAONEs(index, bs)
    case ACHARs(bs, c) => GACHARs(index, bs, c)
    case AALTs(bs, ls) => GAALTs(index, bs, ls.map(g_internalise(_)))
    case ASEQs(bs, r1, r2) => GASEQs(index, bs, g_internalise(r1), g_internalise(r2))
    case ASTARs(bs, r) => GASTARs(index, bs, g_internalise(r))
}}

def getGraph(r: ARexps) : GARexps = {index = -1; g_internalise(r)}

def getNodesCode(g: GARexps) : String = g match{
    case GAZEROs(index) => node(AZEROs, index)
    case GAONEs(index, bs) => node(AONEs(bs), index)
    case GACHARs(index, bs, c) => node(ACHARs(bs, c), index)
    case GAALTs(index, bs, ls) => node(AALTs(bs, List()), index) + "\n" + ls.map(getNodesCode(_)).mkString + "\n"
    case GASEQs(index, bs, r1, r2) => node(ASEQs(bs, AZEROs, AZEROs), index) + "\n" + getNodesCode(r1) + "\n" +  getNodesCode(r2) + "\n"
    case GASTARs(index, bs, r) => node(ASTARs(bs, AZEROs), index) + "\n" + getNodesCode(r) + "\n"
}

def getIndex(g: GARexps) : Int = g match {
    case GAZEROs(index) => index
    case GAONEs(index, _) => index
    case GACHARs(index, _, _) => index
    case GAALTs(index, _, _) => index
    case GASEQs(index, _, _, _) => index
    case GASTARs(index, _, _) => index
}

def getEdgeCode(g: GARexps, parent: Int) : String = g match {
    case GAZEROs(index) => edge(parent, index)
    case GAONEs(index, _) => edge(parent, index)
    case GACHARs(index, _, _) => edge(parent, index)
    case GAALTs(index, _, ls) => edge(parent, index) + "\n" + ls.map(getEdgeCode(_, index)).mkString + "\n"
    case GASEQs(index, _, g1, g2) => edge(parent, index) + "\n" + getEdgeCode(g1, index) + "\n" + getEdgeCode(g2, index) + "\n"
    case GASTARs(index, _, g1) => edge(parent, index) + "\n" + getEdgeCode(g1, index) + "\n"
}

def getEdgeCode(g: GARexps, parent: Int) : String = {
    val edgeString = if(getIndex(g) != 0) edge(parent, getIndex(g)) else ""
    val str = g match {
        case GAZEROs(index) => ""
        case GAONEs(index, _) => ""
        case GACHARs(index, _, _) => ""
        case GAALTs(index, _, ls) =>  "\n" + ls.map(getEdgeCode(_, index)).mkString + "\n"
        case GASEQs(index, _, g1, g2) => "\n" + getEdgeCode(g1, index) + "\n" + getEdgeCode(g2, index) + "\n"
        case GASTARs(index, _, g1) => "\n" + getEdgeCode(g1, index) + "\n"
    }
    edgeString + str
}

def getPythonCode(g: GARexps) : String = getNodesCode(g) + "\n" + getEdgeCode(g, 0) + "\n"

val x0 = "from graphviz import Digraph" + "\n" + "dot = Digraph(comment='The Round Table')" + "\n"

val x1 = getPythonCode(getGraph(test1))

val x2 = "dot.render(\'trees/round-table.gv\', view=True)"

val string = x0+x1+x2

// PrintWriter
import java.io._
val pw = new PrintWriter(new File("code.py" ))
pw.write(string)
pw.close