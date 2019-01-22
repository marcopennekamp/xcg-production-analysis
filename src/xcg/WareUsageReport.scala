package xcg

import scalatags.Text.TypedTag
import scalatags.Text.all._

import scala.io.Source
import Layouts.tdNumber

class WareUsageReport(consumerGoods: Ware, luxuryGoods: Ware) {
  def render(): String = {
    import WareUsages._
    val heading = "Ware Usage Report"
    val document = html(
      head(
        tag("title")(heading),
        tag("style")(Source.fromFile("report.css").mkString)
      ),
      body(
        h1(heading),
        div(renderTable(FinalProductIds, "Final Products Simplified")),
        div(renderTable(FinalProductIds ++ IntermediateIds, "Intermediates Simplified")),
        div(renderTable(FinalProductIds ++ IntermediateIds ++ RefinedIds, "Refined Goods Simplified")),
        div(renderTable(FinalProductIds ++ IntermediateIds ++ RefinedIds ++ RefinedIds2, "Refined Goods and Water Simplified")),
      )
    )

    document.render
  }

  def renderTable(simplifyIds: Set[Id[Ware]], tableName: String): TypedTag[String] = {
    val consumerUsages = WareUsages(consumerGoods.production.resources).simplify(simplifyIds).stacks
    val luxuryUsages = WareUsages(luxuryGoods.production.resources).simplify(simplifyIds).stacks
    val unsimplifiedIds = WareUsages.AllIds -- simplifyIds

    // Includes unused wares and sorts the resulting stack sequence.
    def prepare(stacks: Seq[Stack]): Seq[Stack] = {
      unsimplifiedIds.map { id =>
        stacks.find(_.wareId == id).getOrElse(Stack(id, 0))
      }.toSeq.sortBy(_.ware.name)
    }

    case class Usage(ware: Ware, consumer: Double, luxury: Double) {
      val combined5to1: Double = (consumer * 5 + luxury) / 6
    }
    val usages = prepare(consumerUsages).zip(prepare(luxuryUsages)).map {
      case (consumer, luxury)=> Usage(consumer.ware, consumer.amount, luxury.amount)
    }

    table(
      tr(Seq(th(cls := "column-divider")(b(tableName)), th("Consumer Goods"), th("Luxury Goods"), th("Combined (Ratio 5:1)"))),
      usages.map { usage =>
        tr(Seq(
          td(cls := "column-divider")(usage.ware.name),
          tdNumber(usage.consumer), tdNumber(usage.luxury), tdNumber(usage.combined5to1)
        ))
      }
    )
  }
}

private object WareUsages {
  val FinalProductIds: Set[Id[Ware]] = Set(
    "smartchips", "dronecomponents", "advancedelectronics", "antimatterconverters", "claytronics", "weaponcomponents",
    "missilecomponents", "turretcomponents", "shieldcomponents", "fieldcoils", "foodrations", "medicalsupplies",
    "spacefuel", "nostropoil", "majadust", "spaceweed", "sojahusk", "xcg_passengertransports", "xcg_fingerlasers",
    "xcg_art", "xcg_computers", "xcg_jewelry", "xcg_pets", "xcg_hygienesupplies", "xcg_appliances", "xcg_decoration",
    "xcg_clothing", "xcg_furniture",
  ).map(Id[Ware])

  val IntermediateIds: Set[Id[Ware]] = Set(
    "microchips", "scanningarrays", "hullparts", "advancedcomposites", "engineparts", "plasmaconductors",
    "quantumtubes", "meat", "wheat", "spices", "sunriseflowers", "majasnails", "swampplant", "sojabeans",
    "xcg_paint", "xcg_dyes", "xcg_cloth", "xcg_screens", "xcg_chemicals",
  ).map(Id[Ware])

  val RefinedIds: Set[Id[Ware]] = Set(
    "siliconwafers", "refinedmetals", "teladianium", "graphene", "antimattercells", "superfluidcoolant", "xcg_fiber",
    "xcg_glass", "xcg_preciousmetals", "xcg_wood",
  ).map(Id[Ware])

  // Water is treated on a lower level because of Wood's status as a refined good.
  val RefinedIds2: Set[Id[Ware]] = Set(
    "water",
  ).map(Id[Ware])

  val RawIds: Set[Id[Ware]] = Set(
    "silicon", "ore", "methane", "hydrogen", "helium", "ice", "energycells"
  ).map(Id[Ware])

  val AllIds: Set[Id[Ware]] = RawIds ++ RefinedIds2 ++ RefinedIds ++ IntermediateIds ++ FinalProductIds
}

private case class WareUsages(stacks: Seq[Stack]) {
  def canSimplify(wareIds: Set[Id[Ware]]): Boolean = stacks.map(_.wareId).toSet.intersect(wareIds).nonEmpty

  def simplify(wareIds: Set[Id[Ware]]): WareUsages = {
    assert(wareIds.forall(Wares.get(_).isDefined), "Not all wares to simplify actually exist!")

    if (canSimplify(wareIds)) {
      // Split stacks into their production resources.
      val simplified = stacks.flatMap { stack =>
        if (wareIds.contains(stack.wareId)) {
          stack.ware.production.resourcesPerWare.map(_ * stack.amount)
        } else {
          Seq(stack)
        }
      }

      // Merge stacks with the same wareId.
      val merged = simplified.groupBy(_.wareId).values.map { stacks =>
        val amount = stacks.foldLeft(0.0)(_ + _.amount)
        Stack(stacks.head.wareId, amount)
      }.toSeq

      WareUsages(merged).simplify(wareIds)
    } else {
      this
    }
  }
}
