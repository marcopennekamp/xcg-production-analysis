package xcg

/**
  * An ID type that guarantees that IDs can not be accidentally used as IDs of other, unrelated models.
  *
  * @tparam A The type of the model this type is an ID of.
  */
case class Id[A](value: String) {
  override def toString: String = value.toString
  def isDefined: Boolean = value != Id.Undefined[A].value
}

object Id {
  def Undefined[A]: Id[A] = Id[A]("")
}
