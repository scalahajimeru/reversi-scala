package othello.view

import othello.model.{Board, CellState, GameState, Turn}
import othello.model.Board.CellsOps

import scala.collection.immutable.SortedMap

object GameView {
  def showState(state: GameState): Unit = {
    if (state.gameover) {
      println("ゲーム終了")
      println("黒: " + state.cells.countDarkDisks + "枚")
      println("白: " + state.cells.countLightDisks + "枚")
    }
    else println(state.moveCount + "手目 " + (if (state.turn eq Turn.Dark) "黒"
    else "白") + "の番")
    showBoard(state.cells)
  }

  def showBoard(cells: SortedMap[(Int, Int), CellState]): Unit = {
    println("   a  b  c  d  e  f  g  h")
    println("  +-----------------------+")
    cells.foreach{
      case ((_, _), CellState.Outer) => Unit
      case ((row, 1), cellState) => print(row + " |" + cellState.displayString)
      case ((row, 8), cellState) => println("|" + cellState.displayString + s"|$row\n" +
        "  +--+--+--+--+--+--+--+--+")
      case ((_, _), cellState) => print("|" + cellState.displayString)
    }
    println("   a  b  c  d  e  f  g  h")
  }

}
