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
        renderResourcePerHourTable(resourceUsages),
      ),
      div(
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
      usages.map(_.resourceTop.ware.name),
      usages.map(_.resourceTop.amount.toDouble),
      usages.map(_.resourceBottom.amount.toDouble),
      restrictRatiosTo = Some(Seq.empty)
    ).render()
  }

  private def renderResourcePerHourTable(usages: Seq[ResourceUsage]): TypedTag[String] = {
    ComparisonTableLayout(
      this, "Resources / h",
      usages.map(_.resourceTop.ware.name),
      usages.map(_.topAmountPerHour),
      usages.map(_.bottomAmountPerHour),
      restrictRatiosTo = None
    ).render()
  }

  private def renderResourceCostTable(usages: Seq[ResourceUsage]): TypedTag[String] = {
    ComparisonTableLayout(
      this, "Resource Costs (avg / h)",
      usages.map(_.resourceTop.ware.name),
      usages.map(_.averageTopCostPerHour.truncate),
      usages.map(_.averageBottomCostPerHour.truncate),
      restrictRatiosTo = None,
      unit = Some(X4Unit.Credits),
    ).render()
  }

  private def renderResourceVolumeTable(usages: Seq[ResourceUsage]): TypedTag[String] = {
    ComparisonTableLayout(
      this, "Resource Volume / h",
      usages.map(_.resourceTop.ware.name),
      usages.map(_.topVolumePerHour.truncate),
      usages.map(_.bottomVolumePerHour.truncate),
      restrictRatiosTo = None,
      unit = Some(X4Unit.Volume),
    ).render()
  }

  private def renderProductionTable(): TypedTag[String] = {
    def values(production: Production): Seq[Double] = {
      Seq(production.time, production.cyclesPerHour, production.amount, production.amountPerHour)
    }

    ComparisonTableLayout(
      this, "Production",
      Seq("Time (s)", "Cycles / h", "Amount / cycle", "Amount / h"),
      values(top.production),
      values(bottom.production),
      restrictRatiosTo = Some(Seq(0, 3))
    ).render()
  }

  private def renderProductionVolumeTable(): TypedTag[String] = {
    def values(production: Production): Seq[Double] = {
      Seq(production.resourceVolumePerWare, production.product.volume, production.resourceVolumePerHour,
        production.volumePerHour, production.volumeMultiplier)
    }

    ComparisonTableLayout(
      this, "Production Volume",
      Seq("Res. Vol. / ware", "Vol. / ware", "Res. Vol. / h", "Vol. / h", "Multiplier"),
      values(top.production),
      values(bottom.production),
      restrictRatiosTo = Some(Seq(2, 3)),
      unit = Some(X4Unit.Volume),
    ).render()
  }
}

case class ResourceUsage(comparison: Comparison, resourceTop: Stack, resourceBottom: Stack) {
  private lazy val topCyclesPerHour = comparison.top.production.cyclesPerHour
  private lazy val bottomCyclesPerHour = comparison.bottom.production.cyclesPerHour

  lazy val topAmountPerHour: Double = resourceTop.amount * topCyclesPerHour
  lazy val bottomAmountPerHour: Double = resourceBottom.amount * bottomCyclesPerHour

  lazy val averageTopCostPerHour: Double = resourceTop.value.average * topCyclesPerHour
  lazy val averageBottomCostPerHour: Double = resourceBottom.value.average * bottomCyclesPerHour

  lazy val topVolumePerHour: Double = resourceTop.volume * topCyclesPerHour
  lazy val bottomVolumePerHour: Double = resourceBottom.volume * bottomCyclesPerHour
}
