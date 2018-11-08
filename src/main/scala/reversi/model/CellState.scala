package reversi.model

sealed trait CellState {
  def displayString: String = this match {
    case CellState.Blank => "  "
    case CellState.Outer => "＃"
    case CellState.Light => "○"
    case CellState.Dark => "●"
    case _ => ""
  }

  def reverseDisk: CellState = this match {
    case CellState.Light => CellState.Dark
    case CellState.Dark => CellState.Light
    case _ => this
  }
}

object CellState {
  case object Blank extends CellState
  case object Outer extends CellState
  case object Light extends CellState
  case object Dark extends CellState
}
