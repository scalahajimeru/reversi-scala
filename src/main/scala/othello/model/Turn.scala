package othello.model

sealed trait Turn

object Turn {
  case object Light extends Turn
  case object Dark extends Turn
}
