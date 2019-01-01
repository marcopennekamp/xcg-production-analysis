package xcg

import scalatags.Text.TypedTag
import scalatags.Text.all._

import scala.collection.mutable
import scala.io.Source
import Formatting._
import xcg.Layouts._

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
    val resourceUsages = {
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
      usageMap.values.toSeq
    }

    div(
      h3(s"${top.name} vs. ${bottom.name}"),
      div(
        renderResourceTable(resourceUsages),
        renderResourceCostTable(resourceUsages),
        renderResourceVolumeTable(resourceUsages),
      ),
      div(
        renderProductionTable(),
        renderProductionVolumeTable(),
      ),
    )
  }

  private def renderResourceTable(usages: Seq[ResourceUsage]): TypedTag[String] = {
    ComparisonTableLayout(
      this, "Resources",
      for (usage <- usages) yield th(usage.resourceTop.ware.name),
      for (usage <- usages) yield td(usage.resourceTop.amount),
      for (usage <- usages) yield td(usage.resourceBottom.amount),
      "Amount Ratio", for (usage <- usages) yield td(usage.amountRatio.asRelativePercentage)
    ).render()
  }

  private def renderResourceCostTable(usages: Seq[ResourceUsage]): TypedTag[String] = {
    ComparisonTableLayout(
      this, "Resource Costs (avg / h)",
      for (usage <- usages) yield th(usage.resourceTop.ware.name),
      for (usage <- usages) yield td(usage.averageTopCostPerHour.toInt),
      for (usage <- usages) yield td(usage.averageBottomCostPerHour.toInt),
      "Cost Ratio", for (usage <- usages) yield td(usage.averageCostRatio.asRelativePercentage)
    ).render()
  }

  private def renderResourceVolumeTable(usages: Seq[ResourceUsage]): TypedTag[String] = {
    ComparisonTableLayout(
      this, "Resource Volume / h",
      for (usage <- usages) yield th(usage.resourceTop.ware.name),
      for (usage <- usages) yield td(usage.topVolumePerHour.toInt),
      for (usage <- usages) yield td(usage.bottomVolumePerHour.toInt),
      "Volume Ratio", for (usage <- usages) yield td(usage.volumeRatio.asRelativePercentage)
    ).render()
  }

  private def renderProductionTable(): TypedTag[String] = {
    def values(production: Production): Seq[Modifier] = {
      Seq(
        td(production.time),
        td(production.cyclesPerHour),
        td(production.amount),
        td(production.amountPerHour)
      )
    }

    def ratios(top: Production, bottom: Production): Seq[Modifier] = {
      Seq(
        td((bottom.time.toDouble / top.time).asRelativePercentage),
        td("–"),
        td("–"),
        td((bottom.amountPerHour / top.amountPerHour).asRelativePercentage),
      )
    }

    ComparisonTableLayout(
      this, "Production",
      Seq(th("Time (s)"), th("Cycles / h"), th("Amount / cycle"), th("Amount / h")),
      values(top.production),
      values(bottom.production),
      "Ratio", ratios(top.production, bottom.production)
    ).render()
  }

  private def renderProductionVolumeTable(): TypedTag[String] = {
    def values(production: Production): Seq[Modifier] = {
      Seq(
        td(production.resourceVolumePerWare),
        td(production.resourceVolumePerHour),
        td(production.product.volume),
        td(production.volumePerHour)
      )
    }

    def ratios(top: Production, bottom: Production): Seq[Modifier] = {
      Seq(
        td((bottom.resourceVolumePerWare / top.resourceVolumePerWare).asRelativePercentage),
        td((bottom.resourceVolumePerHour / top.resourceVolumePerHour).asRelativePercentage),
        td("–"),
        td((bottom.volumePerHour / top.volumePerHour).asRelativePercentage),
      )
    }

    ComparisonTableLayout(
      this, "Production Volume",
      Seq(th("Resource Volume / ware"), th("Resource Volume / h"), th("Volume / ware"), th("Volume / h")),
      values(top.production),
      values(bottom.production),
      "Volume Ratio", ratios(top.production, bottom.production)
    ).render()
  }
}

case class ResourceUsage(comparison: Comparison, resourceTop: Stack, resourceBottom: Stack) {
  lazy val amountRatio: Double = {
    val bottomAmountPerHour = resourceBottom.amount * comparison.bottom.production.cyclesPerHour
    val topAmountPerHour = resourceTop.amount * comparison.top.production.cyclesPerHour
    bottomAmountPerHour / topAmountPerHour
  }

  lazy val averageTopCostPerHour: Double = resourceTop.value.average * comparison.top.production.cyclesPerHour
  lazy val averageBottomCostPerHour: Double = resourceBottom.value.average * comparison.bottom.production.cyclesPerHour
  lazy val averageCostRatio: Double = averageBottomCostPerHour / averageTopCostPerHour

  lazy val topVolumePerHour: Double = resourceTop.volume * comparison.top.production.cyclesPerHour
  lazy val bottomVolumePerHour: Double = resourceBottom.volume * comparison.bottom.production.cyclesPerHour
  lazy val volumeRatio: Double = bottomVolumePerHour / topVolumePerHour
}
