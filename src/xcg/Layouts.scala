package xcg

import scalatags.Text._
import scalatags.Text.all._

object Layouts {
  case class ComparisonTableLayout(
    comparison: Comparison,
    tableName: String,
    headers: Seq[Modifier],
    topValues: Seq[Modifier],
    bottomValues: Seq[Modifier],
    ratioName: String,
    ratioValues: Seq[Modifier]
  ) {
    def render(): TypedTag[String] = {
      table(
        tr(Seq(th(cls := "column-divider")(b(tableName))) ++ headers),
        tr(Seq(td(cls := "column-divider")(comparison.top.name)) ++ topValues),
        tr(cls := "row-divider")(Seq(td(cls := "column-divider")(comparison.bottom.name)) ++ bottomValues),
        tr(Seq(td(cls := "column-divider")(ratioName)) ++ ratioValues)
      )
    }
  }
}
