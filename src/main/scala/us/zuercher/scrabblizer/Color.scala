package us.zuercher.scrabblizer

sealed trait Color {
  def code: String

  val toANSI = "\u001B[" + code + "m"
}

object Color {
  case object Reset  extends Color { override val code = "0" }
  case object Blue   extends Color { override val code = "34;1" }
  case object Green  extends Color { override val code = "32;1" }
  case object Red    extends Color { override val code = "31;1" }
  case object Yellow extends Color { override val code = "33;1" }
}

object Colorize {
  private[this] var useColor = false

  def enableColor() {
    useColor = true
  }

  def disableColor() {
    useColor = false
  }

  def withColor(c: Color)(f: => String): String = {
    if (useColor) {
      c.toANSI + f + c.toANSI
    } else {
      f
    }
  }
}
