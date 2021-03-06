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
    val module = new Module(f, m.name)
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
      case Connect(_, lhs: SubField, expr) => expr match {
        // connection of 'SubField', usually a port of memory
        case Reference(_, ClockType, _, _) => Unit  // just ignore
        case _ => {
          val smv.Ref(_, name) = toSmvIR(lhs)
          require(m.getVariable(name) == None, "redefining 'SubField'")
          m.addMemPort(name, toSmvIR(expr))
        }
      }
      case DefMemory(_, name, dtype, depth, wlat, rlat,
                     Seq(reader), Seq(writer), _, _) => {
        m.addMemory(name, toSmvType(dtype), depth,
                    wlat, rlat, reader, writer)
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
    case BundleType(_) => smv.AnyType
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
    case PrimOps.Dshr => (smv.Shr, true)
    case PrimOps.Dshl => (smv.Shl, true)
    case PrimOps.Cat => (smv.Cat, true)
    case PrimOps.Tail => (smv.Nop, false)
    case _ => ???
  }

  // convert expression to SMV IR
  def toSmvIR(e: Expression): IR = e match {
    case DoPrim(op, args, consts, tpe) => op match {
      case PrimOps.Bits => smv.BitsSel(toSmvType(tpe), toSmvIR(args(0)),
                                       consts(0), consts(1))
      case PrimOps.Shl | PrimOps.Shr => {
        val irType = toSmvType(tpe)
        val lhs = toSmvIR(args(0))
        val rhs = smv.WordLiteral(irType, consts(0))
        smv.BinaryExpr(irType, if (op == PrimOps.Shl) smv.Shl else smv.Shr,
                       lhs, rhs)
      }
      case PrimOps.Pad => smv.Pad(toSmvType(tpe), toSmvIR(args(0)), consts(0))
      case _ => toSmvOp(op) match {
        case (sop, true) => smv.BinaryExpr(toSmvType(tpe), sop,
                                           toSmvIR(args(0)),
                                           toSmvIR(args(1)))
        case (sop, false) => smv.UnaryExpr(toSmvType(tpe), sop,
                                           toSmvIR(args(0)))
      }
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
    case SubField(expr, name, _, _) => {
      val smv.Ref(_, ref) = toSmvIR(expr)
      smv.Ref(smv.AnyType, s"$ref.$name")
    }
    case _ => ???
  }
}
