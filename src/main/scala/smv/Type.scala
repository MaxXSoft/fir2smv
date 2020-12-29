package smv

sealed trait Type {
  def width: BigInt
  def asWidth(width: BigInt): Type
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
  override def serialize: String = s"unsigned word[${n}]"
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
  override def serialize: String = s"signed word[${n}]"
}
