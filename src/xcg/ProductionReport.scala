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
  import xcg.Layouts.ComparisonTable._

  lazy val cb = ColumnBuilder(top.production, bottom.production)
  lazy val cbcr = ColumnBuilder(top.production, bottom.production, defaultUnit = Some(X4Unit.Credits))
  lazy val cbv = ColumnBuilder(top.production, bottom.production, defaultUnit = Some(X4Unit.Volume))

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
      div(
        renderPriceMatrix("Cost / ware", _.cost.perWare),
        renderPriceMatrix("Cost / h", _.cost.perHour),
      ),
      div(
        renderPriceMatrix("Price / ware", _.revenue.perWare),
        renderPriceMatrix("Revenue / h", _.revenue.perHour),
      ),
      div(
        renderPriceMatrix("Profit / ware", _.profit.perWare),
        renderPriceMatrix("Profit / h", _.profit.perHour),
      ),
    )
  }

  def usageColumn(usage: ResourceUsage, value: UsageMetrics => Double, showRatio: Boolean = true,
    unit: Option[X4Unit] = None
  ): Column = {
    ColumnBuilder(usage.top, usage.bottom)(usage.resourceName, value, showRatio, unit)
  }

  private def renderResourceTable(usages: Seq[ResourceUsage]): TypedTag[String] = {
    val columns = usages.map(usage => usageColumn(usage, _.amount.perCycle, showRatio = false))
    Layout(this, "Resources", columns).render()
  }

  private def renderResourcePerHourTable(usages: Seq[ResourceUsage]): TypedTag[String] = {
    val columns = usages.map(usage => usageColumn(usage, _.amount.perHour))
    Layout(this, "Resources / h", columns).render()
  }

  private def renderResourceCostTable(usages: Seq[ResourceUsage]): TypedTag[String] = {
    val columns = usages.map(usage => usageColumn(usage, _.averageCost.perHour.truncate, unit = Some(X4Unit.Credits)))
    Layout(this, "Resource Costs (avg / h)", columns).render()
  }

  private def renderResourceVolumeTable(usages: Seq[ResourceUsage]): TypedTag[String] = {
    val columns = usages.map(usage => usageColumn(usage, _.volume.perHour.truncate, unit = Some(X4Unit.Volume)))
    Layout(this, "Resource Volume / h", columns).render()
  }

  private def renderProductionTable(): TypedTag[String] = {
    val columns = Seq(
      cb("Time", _.time, unit = Some(X4Unit.Seconds)),
      cb("Cycles / h", _.cyclesPerHour, showRatio = false),
      cb("Amount / cycle", _.amountMetric.perCycle, showRatio = false),
      cb("Amount / h", _.amountMetric.perHour),
    )

    Layout(this, "Production", columns).render()
  }

  private def renderProductionVolumeTable(): TypedTag[String] = {
    val columns = Seq(
      cbv("Res. Vol. / ware", _.resourceVolume.perWare, showRatio = false),
      cbv("Vol. / ware", _.volume.perWare, showRatio = false),
      cbv("Res. Vol. / h", _.resourceVolume.perHour),
      cbv("Vol. / h", _.volume.perHour),
      cb("Multiplier", _.volumeMultiplier, showRatio = false),
    )

    Layout(this, "Production Volume", columns).render()
  }

  def priceColumns(price: Production => Price): Seq[Column] = Seq(
    cbcr("min", price(_).min),
    cbcr("avg", price(_).average),
    cbcr("max", price(_).max),
  )

  def renderPriceMatrix(title: String, price: Production => Price): TypedTag[String] = {
    Layout(this, title, priceColumns(price)).render()
  }
}

/**
  * Note: The *perWare* metrics are values per PRODUCED ware, not per resource ware.
  */
class UsageMetrics(private val production: Production, private val stack: Stack) {
  private def tdm(value: Double) = TimedDoubleMetric(value, production.amount, production.cyclesPerHour)

  val amount: TimedMetric[Double] = tdm(stack.amount)
  val averageCost: TimedMetric[Double] = tdm(stack.value.average)
  val volume: TimedMetric[Double] = tdm(stack.volume)
}

case class ResourceUsage(comparison: Comparison, resourceTop: Stack, resourceBottom: Stack) {
  lazy val resourceName: String = resourceTop.ware.name

  object top extends UsageMetrics(comparison.top.production, resourceTop)
  object bottom extends UsageMetrics(comparison.bottom.production, resourceBottom)
}
