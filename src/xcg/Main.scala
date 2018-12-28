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
    val wood = Wares.get(Id("xcgwood")).get
    val report = new ProductionReport(wood).render()
    val writer = new PrintWriter(new File("reports/xcgwood.html"))
    writer.write(report)
    writer.close()
  }

}
