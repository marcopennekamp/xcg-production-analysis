package xcg

import io.circe.{Decoder, HCursor}
import io.circe.generic.semiauto._

case class Price(min: Double, average: Double, max: Double) {
  def +(price: Price): Price = Price(this.min + price.min, this.average + price.average, this.max + price.max)
  def -(price: Price): Price = Price(this.min - price.min, this.average - price.average, this.max - price.max)
  def *(price: Price): Price = Price(this.min * price.min, this.average * price.average, this.max * price.max)
  def /(price: Price): Price = Price(this.min / price.min, this.average / price.average, this.max / price.max)
  def *(scalar: Double): Price = this * Price(scalar, scalar, scalar)
  def /(scalar: Double): Price = this / Price(scalar, scalar, scalar)
}

object Price {
  val zero: Price = Price(0, 0, 0)
  implicit val priceDecoder: Decoder[Price] = deriveDecoder[Price]
}

case class Stack(wareId: Id[Ware], amount: Int) {
  lazy val ware: Ware = Wares.get(wareId).get
  lazy val value: Price = ware.price * amount
  lazy val volume: Double = ware.volume * amount
}

object Stack {
  implicit val stackDecoder: Decoder[Stack] = deriveDecoder[Stack]
}

/**
  * @param time The production time in seconds.
  */
case class Production(productId: Id[Ware], time: Int, amount: Int, resources: Seq[Stack]) {
  lazy val product: Ware = Wares.get(productId).get

  lazy val cyclesPerHour: Double = 3600.0 / time

  // Construction shorthands for TimedMetrics.
  private def tdm(value: Double) = TimedDoubleMetric(value, amount, cyclesPerHour)
  private def tpm(value: Price) = TimedPriceMetric(value, amount, cyclesPerHour)

  lazy val amountMetric: TimedMetric[Double] = tdm(amount)
  lazy val volume: TimedMetric[Double] = tdm(product.volume * amount)
  lazy val resourceVolume: TimedMetric[Double] = tdm(resources.map(_.volume).sum)
  lazy val cost: TimedMetric[Price] = tpm(resources.map(_.value).fold(Price.zero)(_ + _))
  lazy val revenue: TimedMetric[Price] = tpm(product.price * amount)

  /**
    * Note that profit is calculated by (min price - max cost, avg price - avg cost, max price - min cost),
    * i.e. taking into account minimum and maximum price spans.
    */
  lazy val profit: TimedMetric[Price] = tpm {
    val min = revenue.perCycle.min - cost.perCycle.max
    val average = revenue.perCycle.average - cost.perCycle.average
    val max = revenue.perCycle.max - cost.perCycle.min
    Price(min, average, max)
  }

  /**
    * The volume of the output compared to the volume of the input (resources). For example, a multiplier of 2.0 means
    * that the product output is twice the volume of the input (e.g. when a product is grown, like wheat or wood).
    * This multiplier is useful to ensure that a product isn't appearing "out of thin air." If the multiplier is above
    * 1.0, there should be a good reason why the product has more volume than its resources.
    */
  lazy val volumeMultiplier: Double = volume.perWare / resourceVolume.perWare
}

object Production {
  def productionDecoder(productId: Id[Ware]): Decoder[Production] = (c: HCursor) => {
    for {
      time <- c.downField("time").as[Int]
      amount <- c.downField("amount").as[Int]
      resources <- c.downField("resources").as[Seq[Stack]]
    } yield Production(productId, time, amount, resources)
  }
}

case class Ware(id: Id[Ware], name: String, volume: Int, price: Price, production: Production)

object Ware {
  implicit val wareDecoder: Decoder[Ware] = (c: HCursor) => {
    for {
      id <- c.downField("id").as[Id[Ware]]
      name <- c.downField("name").as[String]
      volume <- c.downField("volume").as[Int]
      price <- c.downField("price").as[Price]
      production <- c.downField("production").as[Production](Production.productionDecoder(id))
    } yield Ware(id, name, volume, price, production)
  }
}

/**
  * Resource usage metrics based on production and stack values.
  * Note: The *perWare* metrics are values per PRODUCED ware, not per resource ware.
  */
class ResourceUsageMetrics(private val production: Production, private val stack: Stack) {
  private def tdm(value: Double) = TimedDoubleMetric(value, production.amount, production.cyclesPerHour)
  private def tpm(value: Price) = TimedPriceMetric(value, production.amount, production.cyclesPerHour)

  val amount: TimedMetric[Double] = tdm(stack.amount)
  val volume: TimedMetric[Double] = tdm(stack.volume)
  val cost: TimedMetric[Price] = tpm(stack.value)
}
