package xcg

import scalatags.Text._
import scalatags.Text.all._

import Formatting._

object Layouts {
  object ComparisonTable {
    case class Column(
      header: String,
      top: Double,
      bottom: Double,
      showRatio: Boolean = true,
      unit: Option[X4Unit] = None
    )

    case class ColumnBuilder[A](private val top: A, private val bottom: A,
      private val defaultUnit: Option[X4Unit] = None
    ) {
      def apply(header: String, value: A => Double,
        showRatio: Boolean = true, unit: Option[X4Unit] = defaultUnit
      ): Column = {
        Column(header, value(top), value(bottom), showRatio, unit)
      }
    }

    case class Layout(
      comparison: Comparison,
      tableName: String,
      columns: Seq[Column],
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
          val rounded = value.formatRound
          val valueString = if (rounded.isWhole()) rounded.toInt.toString else rounded.toString
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
}
