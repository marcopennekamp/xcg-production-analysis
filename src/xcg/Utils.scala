package xcg

import java.text.NumberFormat

object Utils {
  private val percentInstance = {
    val instance = NumberFormat.getPercentInstance
    instance.setMinimumFractionDigits(1)
    instance.setMaximumFractionDigits(1)
    instance
  }

  implicit class DoubleTools(d: Double) {
    def asPercentage: String = percentInstance.format(d)
    def asRelativePercentage: String = {
      val change = (d - 1.0).asPercentage
      if (d > 1.0) s"+$change" else change
    }
    def truncate: Double = BigDecimal(d).setScale(0, BigDecimal.RoundingMode.HALF_UP).toDouble
    def formatRound: BigDecimal = BigDecimal(d).setScale(2, BigDecimal.RoundingMode.HALF_UP)
  }
}
