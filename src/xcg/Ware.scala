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
  lazy val amountPerHour: Double = amount * cyclesPerHour
  lazy val volumePerHour: Double = product.volume * amountPerHour

  lazy val costPerWare: Price = resources.map(_.value).fold(Price.zero)(_ + _) / amount
  lazy val costPerHour: Price = costPerWare * amountPerHour

  lazy val resourceVolumePerWare: Double = resources.map(_.volume).sum / amount
  lazy val resourceVolumePerHour: Double = resourceVolumePerWare * amountPerHour

  /**
    * The ratio of resource volume compared to product volume. For example, a ratio of 2.0 would mean that
    * the resources have twice the volume of the product.
    */
  lazy val volumeRatio: Double = resourceVolumePerWare / product.volume

  lazy val revenuePerHour: Price = product.price * amountPerHour

  /**
    * Note that the profit is calculated by (min price - max cost, avg price - avg cost, max price - min cost),
    * i.e. taking into account minimum and maximum price spans.
    */
  lazy val profitPerWare: Price = {
    val min = product.price.min - costPerWare.max
    val average = product.price.average - costPerWare.average
    val max = product.price.max - costPerWare.max
    Price(min, average, max)
  }
  lazy val profitPerHour: Price = profitPerWare * amountPerHour
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

case class Ware(id: Id[Ware], volume: Int, price: Price, production: Production) {
  /**
    * This field is mainly present to make refactoring easier should we decide to provide a proper
    * name later (instead of just the ID).
    */
  val name: String = id.toString
}

object Ware {
  implicit val wareDecoder: Decoder[Ware] = (c: HCursor) => {
    for {
      id <- c.downField("id").as[Id[Ware]]
      volume <- c.downField("volume").as[Int]
      price <- c.downField("price").as[Price]
      production <- c.downField("production").as[Production](Production.productionDecoder(id))
    } yield Ware(id, volume, price, production)
  }
}
