package object day16 {
  implicit def narrow(x: Long): Byte = x.toByte

  implicit def toByte(b: Boolean): Long = if (b) 1 else 0
}
