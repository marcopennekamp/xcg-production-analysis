package xcg

import java.io.{File, PrintWriter}

object Main {
  def main(args: Array[String]): Unit = {
    // Load data from wares.xml and xcg-wares.yaml.
    Wares.load()

    // Make a new directory, because PrintWriter doesn't.
    new File("reports").mkdir()

    def writeReport(name: String, report: String): Unit = {
      val writer = new PrintWriter(new File(s"reports/$name.html"))
      writer.write(report)
      writer.close()
    }

    // Generate a production report for every XCG ware.
    for (ware <- Wares.getXCGWares) {
      val report = new ProductionReport(ware).render()
      writeReport(ware.id.toString, report)
      println(s"Created production report for ${ware.name}.")
    }

    // Generate a ware usage report.
    {
      val report = new WareUsageReport(Wares.get(Id("xcg_consumergoods")).get, Wares.get(Id("xcg_luxurygoods")).get).render()
      writeReport("ware_usages", report)
      println("Created ware usage report.")
    }
  }
}
