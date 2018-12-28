package xcg

object Main {

  def main(args: Array[String]): Unit = {
    Wares.load()
    val production = Wares.get(Id("smartchips")).get.production
    println(production.costPerWare)
    println(production.costPerHour)
  }

}
