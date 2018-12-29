package xcg

import java.io.{File, PrintWriter}

object Main {
  def main(args: Array[String]): Unit = {
    Wares.load()
    val production = Wares.get(Id("smartchips")).get.production
    println(production.costPerWare)
    println(production.costPerHour)

    // Make a new directory, because PrintWriter doesn't.
    new File("reports").mkdir()

    // Generate a production report for every XCG ware.
    for (ware <- Wares.getXCGWares) {
      val report = new ProductionReport(ware).render()
      val writer = new PrintWriter(new File(s"reports/${ware.id}.html"))
      writer.write(report)
      writer.close()
      println(s"Created production report for ${ware.name}.")
    }
  }
}
