package smv

// trait of IR
sealed trait IR {
  // type of the current IR
  val irType: Type

  // get reference of the current value
  def reference: String
}

// trait of literals or values
sealed trait Value extends IR

// trait of variables
sealed trait Variable extends IR {
  // name of the current variable
  val name: String
  // connected value
  var connect: Option[IR] = None

  override def reference: String = name

  // declaration of the current variable
  def declaration: String = s"${name}: ${irType.serialize};"

  // assignment expression of the current variable
  def assignment: Option[String] = connect match {
    case Some(value) => Some(s"${name} := ${value.reference};")
    case None => None
  }
}

// word literal
case class WordLiteral(irType: Type, value: BigInt) extends Value {
  override def reference: String = irType match {
    case UnsignedWord(n) => s"0ud${n}_${value}"
    case SignedWord(n) => s"0sd${n}_${value}"
    case _ => ???
  }
}

// boolean literal
case class BooleanLiteral(irType: Type, value: Boolean) extends Value {
  override def reference: String = irType match {
    case Boolean => if (value) { "TRUE" } else { "FALSE" }
    case _ => ???
  }
}

// representing a port
case class Port(irType: Type, name: String) extends Variable

// representing a register
case class Register(irType: Type, name: String,
                    reset: IR, init: IR) extends Variable {
  // check if need to be reset
  private def needReset: Boolean = reset match {
    case _: WordLiteral | _: BooleanLiteral => false
    case _ => true
  }

  // value of reset signal (if it's a literal)
  private def resetValue: Boolean = reset match {
    case WordLiteral(_, value) => value != 0
    case BooleanLiteral(_, value) => value
    case _ => ???
  }

  override def assignment: Option[String] = {
    val nextValue = connect match {
      case Some(value) => value.reference
      case None => name
    }
    if (needReset) {
      Some(s"next(${name}) := case " ++
           s"${reset.reference}: ${init.reference}; " ++
           s"TRUE: ${nextValue}; esac;")
    } else if (resetValue) {
      Some(s"next(${name}) := ${init.reference};")
    } else {
      Some(s"next(${name}) := ${nextValue}")
    }
  }
}

// representing a wire
case class Wire(irType: Type, name: String) extends Variable

// all supported operators
sealed abstract class Op(val name: String)
case object Not extends Op("!")
case object And extends Op("&")
case object Or extends Op("|")
case object Xor extends Op("xor")
case object Xnor extends Op("xnor")
case object Imply extends Op("->")
case object Equiv extends Op("<->")
case object Equal extends Op("=")
case object NotEqual extends Op("!=")
case object Lt extends Op("<")
case object Gt extends Op(">")
case object Le extends Op("<=")
case object Ge extends Op(">=")
case object Neg extends Op("-")
case object Add extends Op("+")
case object Sub extends Op("-")
case object Mul extends Op("*")
case object Div extends Op("/")
case object Mod extends Op("mod")
case object Shr extends Op(">>")
case object Shl extends Op("<<")
case object Cat extends Op("::")

// some internal methods for binary/unary expressions
private object OpExpr {
  def getResized(irType: Type, value: IR): String = {
    if (value.irType.width != irType.width) {
      s"resize(${value.reference}, ${irType.width})"
    } else {
      value.reference
    }
  }
}

// representing a binary expression
case class BinaryExpr(irType: Type, op: Op,
                      lhs: IR, rhs: IR) extends Value {
  private def getResized = OpExpr.getResized(irType, _)
  override def reference: String =
    s"${getResized(lhs)} ${op.name} ${getResized(rhs)}"
}

// representing a unary expression
case class UnaryExpr(irType: Type, op: Op, opr: IR) extends Value {
  private def getResized = OpExpr.getResized(irType, _)
  override def reference: String = s"${op.name}${getResized(opr)}"
}

// representing a mux
case class Mux(irType: Type, cond: IR, tval: IR, fval: IR) extends Value {
  override def reference: String =
    s"case ${cond.reference}: ${tval.reference}; " ++
    s"TRUE: ${fval.reference}; esac"
}
