package xcg

import java.text.NumberFormat

object Formatting {
  private val percentInstance = {
    val instance = NumberFormat.getPercentInstance
    instance.setMinimumFractionDigits(1)
    instance.setMaximumFractionDigits(1)
    instance
  }

  implicit class DoubleAsPercentage(d: Double) {
    def asPercentage: String = percentInstance.format(d)
    def asRelativePercentage: String = {
      val change = (d - 1.0).asPercentage
      if (d > 1.0) s"+$change" else change
    }
  }
}
