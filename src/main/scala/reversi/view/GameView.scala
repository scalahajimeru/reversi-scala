package reversi.view

import reversi.model.{Board, CellState, GameState, Turn}

object GameView {
  def showState(state: GameState): Unit = {
    if (state.gameover) {
      println("ゲーム終了")
      println("黒: " + state.board.countDarkDisks + "枚")
      println("白: " + state.board.countLightDisks + "枚")
    }
    else println(state.moveCount + "手目 " + (if (state.turn eq Turn.Dark) "黒"
    else "白") + "の番")
    showBoard(state.board)
  }

  def showBoard(board: Board): Unit = {
    println("   a  b  c  d  e  f  g  h")
    println("  +-----------------------+")
    board.cells.foreach{
      case ((_, _), CellState.Outer) => Unit
      case ((row, 1), cellState) => print(row + " |" + cellState.displayString)
      case ((row, 8), cellState) => println("|" + cellState.displayString + s"|$row\n" +
        "  +--+--+--+--+--+--+--+--+")
      case ((_, _), cellState) => print("|" + cellState.displayString)
    }
    println("   a  b  c  d  e  f  g  h")
  }

}
