package xcg

import io.circe.Decoder.Result
import io.circe.{Decoder, HCursor}

/**
  * An ID type that guarantees that IDs can not be accidentally used as IDs of other, unrelated models.
  *
  * @tparam A The type of the model this type is an ID of.
  */
case class Id[A](value: String) {
  override def toString: String = value.toString
  def isDefined: Boolean = value != Id.undefined[A].value
}

object Id {
  def undefined[A]: Id[A] = Id[A]("")
  implicit def idDecoder[A]: Decoder[Id[A]] = (c: HCursor) => c.as[String].map(Id[A])
}
