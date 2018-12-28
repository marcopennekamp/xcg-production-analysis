package xcg

object Main {

  def main(args: Array[String]): Unit = {
    Wares.load()
    val production = Wares.get(Id("smartchips")).get.production.get
    println(production.costPerWare)
    println(production.costPerHour)
  }

}
