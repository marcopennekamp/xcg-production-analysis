package xcg

import scalatags.Text.TypedTag
import scalatags.Text.all._

import scala.collection.mutable
import scala.io.Source

import Formatting._

class ProductionReport(ware: Ware) {
  val comparisons: Seq[Comparison] = Wares.compareTos(ware.id).map(compareTo => Comparison(compareTo, ware))

  def render(): String = {
    val heading = s"${ware.name} Production Report"
    val document = html(
      head(
        tag("title")(heading),
        tag("style")(Source.fromFile("report.css").mkString)
      ),
      body(
        h1(heading),
        for (comparison <- comparisons) yield comparison.render()
      )
    )

    document.render
  }
}

/**
  * @param top The 'top' ware, usually the X4 vanilla ware to use as a baseline.
  * @param bottom The 'bottom' ware, usually the XCG ware to analyse.
  */
case class Comparison(top: Ware, bottom: Ware) {
  def render(): TypedTag[String] = {
    // Collect ResourceUsages from both ware productions for resource comparisons.
    val usageMap = mutable.HashMap[Id[Ware], ResourceUsage]()
    top.production.resources.foreach { stack =>
      usageMap.put(stack.wareId, ResourceUsage(this, stack, Stack(stack.wareId, 0)))
    }
    bottom.production.resources.foreach { stack =>
      usageMap.get(stack.wareId) match {
        case None => usageMap.put(stack.wareId, ResourceUsage(this, Stack(stack.wareId, 0), stack))
        case Some(usage) => usageMap.update(stack.wareId, ResourceUsage(this, usage.resourceTop, stack))
      }
    }
    val usages = usageMap.values.toSeq

    div(
      h3(s"${top.name} vs. ${bottom.name}"),
      table(
        tr(
          th(cls := "column-divider")(b("Resources")),
          for (usage <- usages) yield th(usage.resourceTop.ware.name)
        ),
        tr(
          td(cls := "column-divider")(top.name),
          for (usage <- usages) yield td(usage.resourceTop.amount)
        ),
        tr(cls := "row-divider")(
          td(cls := "column-divider")(bottom.name),
          for (usage <- usages) yield td(usage.resourceBottom.amount)
        ),
        tr(
          td(cls := "column-divider")("Amount Ratio"),
          for (usage <- usages) yield td(usage.amountRatio.asRelativePercentage)
        )
      )
    )
  }
}

case class ResourceUsage(comparison: Comparison, resourceTop: Stack, resourceBottom: Stack) {
  lazy val amountRatio: Double = {
    val bottomAmountPerHour = resourceBottom.amount * comparison.bottom.production.cyclesPerHour
    val topAmountPerHour = resourceTop.amount * comparison.top.production.cyclesPerHour
    bottomAmountPerHour / topAmountPerHour
  }
}
