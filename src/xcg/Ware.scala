package xcg

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
}

case class Production(product: Ware, timeSeconds: Int, amount: Int, resources: Seq[Stack]) {
  lazy val cyclesPerHour: Double = 3600.0 / timeSeconds
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
}

case class Stack(wareId: Id[Ware], amount: Int) {
  lazy val ware: Ware = Wares.get(wareId).get
  lazy val value: Price = ware.price * amount
  lazy val volume: Double = ware.volume * amount
}

case class Ware(id: Id[Ware], volume: Int, price: Price) {
  var production: Option[Production] = None
}
