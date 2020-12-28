package smv

sealed trait Type {
  def serialize: String
}

// boolean type
case object Boolean extends Type {
  override def serialize: String = "boolean"
}

// unsigned word[N] type
case class UnsignedWord(n: BigInt) extends Type {
  override def serialize: String = s"unsigned word[${n}]"
}

// signed word[N] type
case class SignedWord(n: BigInt) extends Type {
  override def serialize: String = s"signed word[${n}]"
}
