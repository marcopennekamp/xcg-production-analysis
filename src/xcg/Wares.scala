package xcg

import io.circe.Decoder
import io.circe.generic.semiauto._
import io.circe.yaml.parser

import scala.collection.mutable
import scala.io.Source
import scala.xml.{Node, XML}

object Wares {
  private val wares: mutable.HashMap[Id[Ware], Ware] = mutable.HashMap.empty
  private val xcgWares: mutable.ArrayBuffer[Ware] = mutable.ArrayBuffer.empty
  private val compareTo: mutable.HashMap[Id[Ware], Seq[Id[Ware]]] = mutable.HashMap.empty

  private def loadX4Wares(): Unit = {
    val element = XML.loadFile("wares.xml")

    // Filter and only include ware nodes which have "economy" as one of their tags.
    val wareNodes = (element \ "ware").filter { node =>
      node.attribute("tags").fold(false)(tags => tags.head.text.contains("economy"))
    }

    def parseWare(node: Node): Ware = {
      val wareId = Id[Ware](node \@ "id")
      val priceNode = node \ "price"
      val price = Price((priceNode \@ "min").toInt, (priceNode \@ "average").toInt, (priceNode \@ "max").toInt)
      val maybeProduction = (node \ "production").find(_ \@ "method" == "default").map { node =>
        val resources = ((node \ "primary") \ "ware").map { resourceNode =>
          Stack(Id(resourceNode \@ "ware"), (resourceNode \@ "amount").toInt)
        }
        Production(wareId, (node \@ "time").toInt, (node \@ "amount").toInt, resources)
      }
      // Use an empty production for the few edge cases in which a production is not defined (only mined wares, I think).
      // It's not worth it to handle an Option for a few productions which we will not even access in the code.
      val production = maybeProduction.getOrElse(Production(wareId, 0, 0, Seq.empty))

      // We can't provide a proper name yet (we'd also need to parse the localisation files), so we'll use the ID
      // for now.
      Ware(wareId, name = wareId.value, (node \@ "volume").toInt, price, production)
    }

    wareNodes.map(parseWare).foreach(ware => wares.put(ware.id, ware))
  }

  private def loadXCGWares(): Unit = {
    def fail(message: String, underlying: Throwable): Unit = {
      throw new RuntimeException(s"Parsing xcg-wares.yaml failed with the following error:\n${ message }", underlying)
    }

    val maybeJson = parser.parse(Source.fromFile("xcg-wares.yaml").mkString)
    maybeJson match {
      case Left(failure) => fail(failure.message, failure.underlying)
      case Right(json) =>
        // First, we load the wares definitions into the general wares directory.
        json.as[List[Ware]].fold(
          failure => fail(failure.message, failure.getCause),
          list => list.foreach { ware =>
            wares.put(ware.id, ware)
            xcgWares += ware
          }
        )

        // Then, we add the additional parameters defined only for XCG wares into their respective maps.
        case class Options(id: Id[Ware], compareTo: List[Id[Ware]])
        implicit val optionsDecoder: Decoder[Options] = deriveDecoder[Options]

        json.as[List[Options]].fold(
          failure => fail(failure.toString, failure.getCause),
          options => options.foreach(option => compareTo.put(option.id, option.compareTo))
        )
    }
  }

  def load(): Unit = {
    loadX4Wares()
    loadXCGWares()
  }

  def get(id: Id[Ware]): Option[Ware] = {
    val opt = wares.get(id)
    if (opt.isEmpty) {
      println(s"Can't find ware $id.")
    }
    opt
  }
  def compareTos(id: Id[Ware]): Seq[Ware] = compareTo.getOrElse(id, Seq.empty).map(Wares.get).filter(_.isDefined).map(_.get)

  def getXCGWares: Seq[Ware] = xcgWares
}
