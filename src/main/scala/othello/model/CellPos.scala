package othello.model

// immutable
case class CellPos(row: Int, col: Int) {
  def +(pos: CellPos) = CellPos(row + pos.row, col + pos.col)
}
