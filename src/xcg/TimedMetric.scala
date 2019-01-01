package xcg

trait TimedMetric[A] {
  val perWare: A
  val perCycle: A
  val perHour: A
}

/**
  * @param productionAmount The amount produced per cycle.
  */
case class TimedDoubleMetric(value: Double, productionAmount: Int, cyclesPerHour: Double) extends TimedMetric[Double] {
  override val perCycle: Double = value
  override val perWare: Double = perCycle / productionAmount
  override val perHour: Double = perCycle * cyclesPerHour
}

object TimedDoubleMetric {
  def constructor(production: Production): Double => TimedDoubleMetric = {
    value => TimedDoubleMetric(value, production.amount, production.cyclesPerHour)
  }
}

/**
  * @param productionAmount The amount produced per cycle.
  */
case class TimedPriceMetric(value: Price, productionAmount: Int, cyclesPerHour: Double) extends TimedMetric[Price] {
  override val perCycle: Price = value
  override val perWare: Price = perCycle / productionAmount
  override val perHour: Price = perCycle * cyclesPerHour
}

object TimedPriceMetric {
  def constructor(production: Production): Price => TimedPriceMetric = {
    value => TimedPriceMetric(value, production.amount, production.cyclesPerHour)
  }
}
