package xcg

import scala.collection.mutable

object Wares {
  private val wares: mutable.HashMap[Id[Ware], Ware] = mutable.HashMap.empty

  def setup(): Unit = {

  }

  def get(id: Id[Ware]): Option[Ware] = wares.get(id)
}
