package xcg

import scalatags.Text._
import scalatags.Text.all._

import Formatting._

object Layouts {
  case class ComparisonTableLayout(
    comparison: Comparison,
    tableName: String,
    headers: Seq[String],
    topValues: Seq[Double],
    bottomValues: Seq[Double],
    // Which ratios to calculate and show (as indices from 0 to headers.length, exclusive). If the value is None,
    // all ratios are shown. If the value is the empty sequence, no ratios are shown.
    restrictRatiosTo: Option[Seq[Int]] = None,
    unit: Option[X4Unit] = None,
  ) {
    def render(): TypedTag[String] = {
      // Calculate all ratios to be displayed.
      val ratios = topValues.zip(bottomValues).zipWithIndex.map { case ((topValue, bottomValue), index) =>
        // Add a ratio if 'displayedRatios' is None or the index is in the sequence.
        if (restrictRatiosTo.forall(_.contains(index))) {
          td((bottomValue / topValue).asRelativePercentage)
        } else {
          td("–")
        }
      }

      val ratioName = if (restrictRatiosTo.exists(_.isEmpty)) "–" else "Ratio"

      /**
        * Formats the value in a human-readable fashion.
        */
      def tdValue(value: Double): Modifier = {
        val valueString = if (value.isWhole()) value.toInt.toString else value.formatRound
        unit.map(unit => td(valueString, " ", unit.toHtml)).getOrElse(td(valueString))
      }

      table(
        tr(Seq(th(cls := "column-divider")(b(tableName))) ++ headers.map(th(_))),
        tr(Seq(td(cls := "column-divider")(comparison.top.name)) ++ topValues.map(tdValue)),
        tr(cls := "row-divider")(Seq(td(cls := "column-divider")(comparison.bottom.name)) ++ bottomValues.map(tdValue)),
        tr(Seq(td(cls := "column-divider")(ratioName)) ++ ratios)
      )
    }
  }
}
