package smv

sealed trait Type {
  def width: BigInt
  def asWidth(width: BigInt): Type
  def shortName: String
  def serialize: String
}

// boolean type
case object Boolean extends Type {
  override def width: BigInt = 1
  override def asWidth(width: BigInt): Type = {
    if (width == 1) {
      Boolean
    } else {
      UnsignedWord(width)
    }
  }
  override def shortName: String = "b"
  override def serialize: String = "boolean"
}

// unsigned word[N] type
case class UnsignedWord(n: BigInt) extends Type {
  override def width: BigInt = n
  override def asWidth(width: BigInt): Type = {
    if (width == 1) {
      Boolean
    } else {
      UnsignedWord(width)
    }
  }
  override def shortName: String = s"u$n"
  override def serialize: String = s"unsigned word[$n]"
}

// signed word[N] type
case class SignedWord(n: BigInt) extends Type {
  override def width: BigInt = n
  override def asWidth(width: BigInt): Type = {
    if (width == 1) {
      Boolean
    } else {
      SignedWord(width)
    }
  }
  override def shortName: String = s"s$n"
  override def serialize: String = s"signed word[$n]"
}

// any other types
// no operations can be performed on this type
case object AnyType extends Type {
  override def width: BigInt = 0
  override def asWidth(width: BigInt): Type = AnyType
  override def shortName: String = ???
  override def serialize: String = ???
}
