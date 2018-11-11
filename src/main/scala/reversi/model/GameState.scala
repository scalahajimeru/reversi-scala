package reversi.model

import scala.collection.immutable.SortedMap


object GameState {
  def turnToState(turn: Turn): CellState = if (turn == Turn.Light) CellState.Light else CellState.Dark
}

/**
  * ゲームの状態管理を行います。
  * @param board 盤面の状態
  * @param turn 現在手を打とうとしているプレイヤー
  * @param moveCount 現在の手数
  * @param gameover ゲームオーバーになっているか。
  */
case class GameState(board: Board = Board(),
                     turn: Turn = Turn.Dark,
                     moveCount: Int = 1,
                     gameover: Boolean = false) {

  def switchTurn(): GameState = copy(turn = if (turn eq Turn.Dark) Turn.Light else Turn.Dark)

  def pass(): GameState = switchTurn()

  def initialize(): GameState = GameState()

}
