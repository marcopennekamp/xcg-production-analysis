package xcg

case class Price(min: Int, avg: Int, max: Int)

case class Production(ware: Ware, timeSeconds: Int, amount: Int, resources: Seq[Stack]) {
  lazy val cyclesPerHour: Double = 3600.0 / timeSeconds
  lazy val amountPerHour: Double = amount * cyclesPerHour
  lazy val volumePerHour: Double = ware.volume * amountPerHour
}

case class Stack(ware: Ware, amount: Int)

case class Ware(id: Id[Ware], volume: Int, price: Price) {

}
