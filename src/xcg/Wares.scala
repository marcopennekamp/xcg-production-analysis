package xcg

import io.circe.Decoder
import io.circe.generic.semiauto._
import io.circe.yaml.parser

import scala.collection.mutable
import scala.io.Source
import scala.xml.{Node, XML}

object Wares {
  private val wares: mutable.HashMap[Id[Ware], Ware] = mutable.HashMap.empty
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
      val production = (node \ "production").find(_ \@ "method" == "default").map { node =>
        val resources = ((node \ "primary") \ "ware").map { resourceNode =>
          Stack(Id(resourceNode \@ "ware"), (resourceNode \@ "amount").toInt)
        }
        Production(wareId, (node \@ "time").toInt, (node \@ "amount").toInt, resources)
      }
      Ware(wareId, (node \@ "volume").toInt, price, production)
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
          xcgWares => xcgWares.foreach(ware => wares.put(ware.id, ware))
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

  def get(id: Id[Ware]): Option[Ware] = wares.get(id)
  def compareTo(id: Id[Ware]): Seq[Ware] = compareTo.getOrElse(id, Seq.empty).map(Wares.get).filter(_.isDefined).map(_.get)
}
