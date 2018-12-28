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

case class Production(ware: Ware, timeSeconds: Int, amount: Int, resources: Seq[Stack]) {
  lazy val cyclesPerHour: Double = 3600.0 / timeSeconds
  lazy val amountPerHour: Double = amount * cyclesPerHour
  lazy val volumePerHour: Double = ware.volume * amountPerHour

  lazy val costPerWare: Price = resources.map(_.value).fold(Price.zero)(_ + _) / amount
  lazy val costPerHour: Price = costPerWare * amountPerHour
}

case class Stack(wareId: Id[Ware], amount: Int) {
  lazy val ware: Ware = Wares.get(wareId).get
  lazy val value: Price = ware.price * amount
}

case class Ware(id: Id[Ware], volume: Int, price: Price) {
  var production: Option[Production] = None
}
