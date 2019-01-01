package xcg

import scalatags.Text._
import scalatags.Text.all._

import Formatting._

object Layouts {
  case class ComparisonTableColumn(
    header: String,
    top: Double,
    bottom: Double,
    showRatio: Boolean = true,
    unit: Option[X4Unit] = None
  )

  case class ComparisonTableColumnBuilder[A](private val top: A, private val bottom: A,
    private val defaultUnit: Option[X4Unit] = None
  ) {
    def apply(header: String, value: A => Double,
      showRatio: Boolean = true, unit: Option[X4Unit] = defaultUnit
    ): ComparisonTableColumn = {
      ComparisonTableColumn(header, value(top), value(bottom), showRatio, unit)
    }
  }

  case class ComparisonTableLayout(
    comparison: Comparison,
    tableName: String,
    columns: Seq[ComparisonTableColumn],
  ) {
    def render(): TypedTag[String] = {
      val ratioName = if (columns.exists(_.showRatio)) "Ratio" else "–"
      val ratios = columns.map { column =>
        if (column.showRatio) {
          td((column.bottom / column.top).asRelativePercentage)
        } else {
          td("–")
        }
      }

      /**
        * Formats the value in a human-readable fashion.
        */
      def tdValue(value: Double, unit: Option[X4Unit]): Modifier = {
        val valueString = if (value.isWhole()) value.toInt.toString else value.formatRound
        unit.map(unit => td(valueString, " ", unit.toHtml)).getOrElse(td(valueString))
      }

      table(
        tr(Seq(th(cls := "column-divider")(b(tableName))) ++ columns.map(_.header).map(th(_))),
        tr(Seq(td(cls := "column-divider")(comparison.top.name)) ++ columns.map(c => tdValue(c.top, c.unit))),
        tr(cls := "row-divider")(Seq(td(cls := "column-divider")(comparison.bottom.name)) ++
          columns.map(c => tdValue(c.bottom, c.unit))),
        tr(Seq(td(cls := "column-divider")(ratioName)) ++ ratios)
      )
    }
  }
}
