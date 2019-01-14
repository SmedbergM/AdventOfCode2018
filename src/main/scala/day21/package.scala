package object day21 {
  implicit def narrow(j: Int): Byte = j.toByte

  implicit def toInt(b: Boolean): Int = if (b) 1 else 0
}
