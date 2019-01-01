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
  lazy val cb = ComparisonTableColumnBuilder(top.production, bottom.production)

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
    val columns = usages.map { usage =>
      ComparisonTableColumn(usage.resourceName, usage.resourceTop.amount, usage.resourceBottom.amount,
        showRatio = false)
    }

    ComparisonTableLayout(this, "Resources", columns).render()
  }

  private def renderResourcePerHourTable(usages: Seq[ResourceUsage]): TypedTag[String] = {
    val columns = usages.map { usage =>
      ComparisonTableColumn(usage.resourceName, usage.topAmountPerHour, usage.bottomAmountPerHour)
    }

    ComparisonTableLayout(this, "Resources / h", columns).render()
  }

  private def renderResourceCostTable(usages: Seq[ResourceUsage]): TypedTag[String] = {
    val columns = usages.map { usage =>
      ComparisonTableColumn(usage.resourceName, usage.averageTopCostPerHour.truncate,
        usage.averageBottomCostPerHour.truncate, unit = Some(X4Unit.Credits))
    }

    ComparisonTableLayout(this, "Resource Costs (avg / h)", columns).render()
  }

  private def renderResourceVolumeTable(usages: Seq[ResourceUsage]): TypedTag[String] = {
    val columns = usages.map { usage =>
      ComparisonTableColumn(usage.resourceName, usage.topVolumePerHour.truncate,
        usage.bottomVolumePerHour.truncate, unit = Some(X4Unit.Volume))
    }

    ComparisonTableLayout(this, "Resource Volume / h", columns).render()
  }

  private def renderProductionTable(): TypedTag[String] = {
    val columns = Seq(
      cb("Time (s)", _.time, unit = Some(X4Unit.Seconds)),
      cb("Cycles / h", _.cyclesPerHour, showRatio = false),
      cb("Amount / cycle", _.amount, showRatio = false),
      cb("Amount / h", _.amountPerHour),
    )

    ComparisonTableLayout(this, "Production", columns).render()
  }

  private def renderProductionVolumeTable(): TypedTag[String] = {
    val volume = Some(X4Unit.Volume)
    val columns = Seq(
      cb("Res. Vol. / ware", _.resourceVolumePerWare, showRatio = false, unit = volume),
      cb("Vol. / ware", _.product.volume, showRatio = false, unit = volume),
      cb("Res. Vol. / h", _.resourceVolumePerHour, unit = volume),
      cb("Vol. / h", _.volumePerHour, unit = volume),
      cb("Multiplier", _.volumeMultiplier, showRatio = false),
    )

    ComparisonTableLayout(this, "Production Volume", columns).render()
  }
}

case class ResourceUsage(comparison: Comparison, resourceTop: Stack, resourceBottom: Stack) {
  lazy val resourceName: String = resourceTop.ware.name

  private lazy val topCyclesPerHour = comparison.top.production.cyclesPerHour
  private lazy val bottomCyclesPerHour = comparison.bottom.production.cyclesPerHour

  lazy val topAmountPerHour: Double = resourceTop.amount * topCyclesPerHour
  lazy val bottomAmountPerHour: Double = resourceBottom.amount * bottomCyclesPerHour

  lazy val averageTopCostPerHour: Double = resourceTop.value.average * topCyclesPerHour
  lazy val averageBottomCostPerHour: Double = resourceBottom.value.average * bottomCyclesPerHour

  lazy val topVolumePerHour: Double = resourceTop.volume * topCyclesPerHour
  lazy val bottomVolumePerHour: Double = resourceBottom.volume * bottomCyclesPerHour
}
