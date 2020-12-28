package emitter

import firrtl._
import firrtl.ir._

import smv.{File, Module, Boolean, UnsignedWord, SignedWord}

// emit SMV code for model checking
object SmvEmitter {
  def apply(circuit: Circuit): File = {
    val file = new File
    circuit.mapModule(walkModule(file))
    file
  }

  def walkModule(f: File)(m: DefModule): DefModule = {
    val module = new Module(m.name)
    // traverse all ports
    m.mapPort(walkPort(module))
    // traverse all statements
    m.mapStmt(walkStatement(module))
    // add to file
    f.addModule(module)
    m
  }

  def walkPort(m: Module)(p: Port): Port = {
    p.tpe match {
      case ClockType => m.checkClock(p.name)
      case other => m.addPort(p.name, toSmvType(other))
    }
    p
  }

  def walkStatement(m: Module)(s: Statement): Statement = {
    s match {
      case Block(_) => s.mapStmt(walkStatement(m))
      case DefRegister(info, name, tpe, _, reset, init) => {
        // TODO: use info
      }
      case DefNode(info, name, value) => {
        //
      }
      case Connect(info, loc, expr) => {
        //
      }
    }
    s
  }

  def walkExpression(e: Expression): Expression = {
    val visited = e.mapExpr(walkExpression)
    println(visited.tpe)
    visited
  }

  def toSmvType(t: firrtl.ir.Type): smv.Type = t match {
    case UIntType(IntWidth(n)) => if (n == 1) {
      Boolean
    } else {
      UnsignedWord(n)
    }
    case SIntType(IntWidth(n)) => SignedWord(n)
  }
}
