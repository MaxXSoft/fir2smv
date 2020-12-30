package smv

import chisel3.{RawModule, Bits}

package object ltl {

import scala.language.implicitConversions

// trait of all LTL values
sealed trait Value {
  def serialize: String

  // overload operators
  def unary_! : Value = UnaryExpr(Not, this)
  def &(that: Value): Value = BinaryExpr(And, this, that)
  def |(that: Value): Value = BinaryExpr(Or, this, that)
  def ^(that: Value): Value = BinaryExpr(Xor, this, that)
  def ^!(that: Value): Value = BinaryExpr(Xnor, this, that)
  def ->(that: Value): Value = BinaryExpr(Imply, this, that)
  def <->(that: Value): Value = BinaryExpr(Equiv, this, that)
  def ===(that: Value): Value = BinaryExpr(Equal, this, that)
  def !==(that: Value): Value = BinaryExpr(NotEqual, this, that)
  def <(that: Value): Value = BinaryExpr(Lt, this, that)
  def >(that: Value): Value = BinaryExpr(Gt, this, that)
  def <=(that: Value): Value = BinaryExpr(Le, this, that)
  def >=(that: Value): Value = BinaryExpr(Ge, this, that)
  def unary_- : Value = UnaryExpr(Neg, this)
  def +(that: Value): Value = BinaryExpr(Add, this, that)
  def -(that: Value): Value = BinaryExpr(Sub, this, that)
  def *(that: Value): Value = BinaryExpr(Mul, this, that)
  def /(that: Value): Value = BinaryExpr(Div, this, that)
  def %(that: Value): Value = BinaryExpr(Mod, this, that)
  def >>(that: Value): Value = BinaryExpr(Shr, this, that)
  def <<(that: Value): Value = BinaryExpr(Shl, this, that)
  def ::(that: Value): Value = BinaryExpr(Cat, this, that)
  def U(that: Value): Value = BinaryExpr(Until, this, that)
}

// 'Next' operator
object X {
  def apply(that: Value): Value = UnaryExpr(Next, that)
}

// 'Globally' operator
object G {
  def apply(that: Value): Value = UnaryExpr(Global, that)
}

// 'Finally' operator
object F {
  def apply(that: Value): Value = UnaryExpr(Final, that)
}


// reference of a 'chisel3.Bits'
case class Ref(name: String) extends Value {
  override def serialize: String = name
}

object Ref {
  import scala.reflect.macros.blackbox.Context
  import scala.language.experimental.macros

  def apply[P <: RawModule](f: P => Bits): Ref = macro applyImpl[P]

  def applyImpl[P <: RawModule](c: Context)(f: c.Expr[P => Bits]):
      c.Expr[Ref] = {
    import c.universe._

    def flattenSelect(t: Tree): String = t match {
      case Select(Ident(_), TermName(r)) => r
      case Select(l, TermName(r)) => s"${flattenSelect(l)}_$r"
    }

    val Function(List(ValDef(_, _, tpe, _)), body) = f.tree
    val moduleName = tpe.toString.split("\\.").last
    val portName = flattenSelect(body)
    val ref = s"${File.getInstanceName(moduleName)}.${portName}"
    c.Expr(q"new Ref(${ref})")
  }
}

// reference of any value
case class AnyRef(name: String) extends Value {
  override def serialize: String = name
}

object AnyRef {
  import scala.reflect.ClassTag

  def apply[T <: RawModule: ClassTag](name: String): AnyRef = {
    val moduleName = implicitly[ClassTag[T]].runtimeClass.getSimpleName
    new AnyRef(s"${File.getInstanceName(moduleName)}.$name")
  }
}

// binary operation
case class BinaryExpr(op: Op, lhs: Value, rhs: Value) extends Value {
  def serialize: String = s"(${lhs.serialize} ${op.name} ${rhs.serialize})"
}

// unary operation
case class UnaryExpr(op: Op, opr: Value) extends Value {
  def serialize: String = s"${op.name} ${opr.serialize}"
}

// boolean value
case class Bool(value: Boolean) extends Value {
  def serialize: String = if (value) "TRUE" else "FALSE"
}

// unsigned word value
case class UWord(value: BigInt, width: BigInt) extends Value {
  def serialize: String = s"0ud${width}_$value"
}

// signed word value
case class SWord(value: BigInt, width: BigInt) extends Value {
  def serialize: String = s"0sd${width}_$value"
}

// implicit converters
implicit class FromBigIntToLtlValue(value: BigInt) {
  def LU(width: BigInt): UWord = {
    require(width > 1, "width must be greater than 1")
    new UWord(value, width)
  }

  def LS(width: BigInt): SWord = {
    require(width > 1, "width must be greater than 1")
    new SWord(value, width)
  }
}

implicit class FromIntToLtlValue(value: Int)
         extends FromBigIntToLtlValue(value)

implicit class FromLongToLtlValue(value: Long)
         extends FromBigIntToLtlValue(value)

implicit class FromBooleanToLtlValue(value: Boolean) {
  def LB: Bool = new Bool(value)
}

}  // package ltl
