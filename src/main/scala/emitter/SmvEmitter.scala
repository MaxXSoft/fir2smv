package emitter

import firrtl._
import firrtl.ir._

import smv.{File, Module}
import smv.{Boolean, UnsignedWord, SignedWord}
import smv.{IR, Variable, Op}

// emit SMV code for model checking
object SmvEmitter {
  def apply(circuit: Circuit): File = {
    val file = new File
    circuit.mapModule(walkModule(file))
    file
  }

  // callback for traversing modules
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

  // callback for traversing ports
  def walkPort(m: Module)(p: Port): Port = {
    p.tpe match {
      case ClockType => m.checkClock(p.name)
      case other => m.addPort(p.name, toSmvType(other))
    }
    p
  }

  // callback for traversing statements
  def walkStatement(m: Module)(s: Statement): Statement = {
    s match {
      case Block(_) => s.mapStmt(walkStatement(m))
      case Connect(_, Reference(name, _, _, _), expr) => {
        m.getVariable(name).get.connect = Some(toSmvIR(expr))
      }
      case DefNode(_, name, value) => m.addWire(name, toSmvIR(value))
      case DefRegister(_, name, tpe, _, reset, init) => {
        m.addReg(name, toSmvType(tpe), toSmvIR(reset), toSmvIR(init))
      }
    }
    s
  }

  // convert Firrtl type to SMV type
  def toSmvType(t: firrtl.ir.Type): smv.Type = t match {
    case UIntType(IntWidth(n)) => if (n == 1) Boolean else UnsignedWord(n)
    case SIntType(IntWidth(n)) => SignedWord(n)
  }

  // convert 'PrimOp' to SMV 'Op'
  // returns (Smv.Op, IsBinOp)
  def toSmvOp(op: PrimOp): (Op, Boolean) = op match {
    case PrimOps.Not => (smv.Not, false)
    case PrimOps.And => (smv.And, true)
    case PrimOps.Or => (smv.Or, true)
    case PrimOps.Xor => (smv.Xor, true)
    case PrimOps.Eq => (smv.Equal, true)
    case PrimOps.Neq => (smv.NotEqual, true)
    case PrimOps.Lt => (smv.Lt, true)
    case PrimOps.Gt => (smv.Gt, true)
    case PrimOps.Leq => (smv.Le, true)
    case PrimOps.Geq => (smv.Ge, true)
    case PrimOps.Neg => (smv.Neg, false)
    case PrimOps.Add => (smv.Add, true)
    case PrimOps.Sub => (smv.Sub, true)
    case PrimOps.Mul => (smv.Mul, true)
    case PrimOps.Div => (smv.Div, true)
    case PrimOps.Rem => (smv.Mod, true)
    case PrimOps.Shr | PrimOps.Dshr => (smv.Shr, true)
    case PrimOps.Shl | PrimOps.Dshl => (smv.Shl, true)
    case PrimOps.Cat => (smv.Cat, true)
    case PrimOps.Tail => (smv.Nop, false)
    case _ => ???
  }

  // convert expression to SMV IR
  def toSmvIR(e: Expression): IR = e match {
    case DoPrim(op, args, _, tpe) => toSmvOp(op) match {
      case (sop, true) => smv.BinaryExpr(toSmvType(tpe), sop,
                                         toSmvIR(args(0)),
                                         toSmvIR(args(1)))
      case (sop, false) => smv.UnaryExpr(toSmvType(tpe), sop,
                                         toSmvIR(args(0)))
    }
    case Mux(cond, tval, fval, tpe) => smv.Mux(toSmvType(tpe),
                                               toSmvIR(cond),
                                               toSmvIR(tval),
                                               toSmvIR(fval))
    case Reference(name, tpe, _, _) => smv.Ref(toSmvType(tpe), name)
    case SIntLiteral(value, IntWidth(w)) => smv.WordLiteral(SignedWord(w),
                                                            value)
    case UIntLiteral(value, IntWidth(w)) => {
      if (w == 1) {
        smv.BooleanLiteral(Boolean, value != 0)
      } else {
        smv.WordLiteral(UnsignedWord(w), value)
      }
    }
    case _ => ???
  }
}
