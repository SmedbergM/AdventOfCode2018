package common

trait AdventApp extends App {
  val sessionId = sys.env("SESSION_ID")
}
