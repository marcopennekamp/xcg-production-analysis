package xcg

import scala.collection.mutable
import scala.xml.{Node, XML}

object Wares {
  private val wares: mutable.HashMap[Id[Ware], Ware] = mutable.HashMap.empty

  def load(): Unit = {
    val element = XML.loadFile("wares.xml")

    // Filter and only include ware nodes which have "economy" as one of their tags.
    val wareNodes = (element \ "ware").filter { node =>
      node.attribute("tags").fold(false)(tags => tags.head.text.contains("economy"))
    }

    def parseWare(node: Node): Ware = {
      val priceNode = node \ "price"
      val price = Price((priceNode \@ "min").toInt, (priceNode \@ "average").toInt, (priceNode \@ "max").toInt)
      val ware = Ware(Id(node \@ "id"), (node \@ "volume").toInt, price)
      val productionNode = (node \ "production").find(_ \@ "method" == "default")
      ware.production = productionNode.map { node =>
        val resources = ((node \ "primary") \ "ware").map { resourceNode =>
          Stack(Id(resourceNode \@ "ware"), (resourceNode \@ "amount").toInt)
        }
        Production(ware, (node \@ "time").toInt, (node \@ "amount").toInt, resources)
      }
      ware
    }

    wareNodes.map(parseWare).foreach(ware => wares.put(ware.id, ware))
  }

  def get(id: Id[Ware]): Option[Ware] = wares.get(id)
}
