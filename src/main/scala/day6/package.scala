package object day6 {
  implicit class RichLocation(t: (Int, Int)) {
    def distance(other: (Int, Int)): Int = {
      math.abs(t._1 - other._1) + math.abs(t._2 - other._2)
    }

    def +(other: (Int, Int)): (Int, Int) = (t._1 + other._1, t._2 + other._2)
  }
}
