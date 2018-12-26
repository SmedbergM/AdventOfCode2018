package object day15 {
  type NonEmptyList[T] = ::[T]

  def nonEmptyList[T](t: T): NonEmptyList[T] = asNonEmptyList(t :: Nil)

  def asNonEmptyList[T](ts: List[T]): NonEmptyList[T] = ts.asInstanceOf[NonEmptyList[T]]
}
