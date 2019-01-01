package xcg

import scalatags.Text.all._

trait X4Unit {
  def toHtml: Modifier
}

object X4Unit {
  case object Credits extends X4Unit {
    override def toHtml: Modifier = span(color := "darkgoldenrod")("cr")
  }
  case object Volume extends X4Unit {
    override def toHtml: Modifier = span(color := "darkgreen")("m", sup("3"))
  }
  case object Seconds extends X4Unit {
    override def toHtml: Modifier = span(color := "slateblue")("s")
  }
}
