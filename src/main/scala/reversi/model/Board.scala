package reversi.model

import scala.collection.immutable.{SortedMap, TreeMap}


/**
  * 盤面の状態管理を行います。
  *
  */
object Board {
  private def initialize(): SortedMap[(Int, Int), CellState] = TreeMap(
    (for (row <- 0 to 9; col <- 0 to 9) yield {
      (row, col) match {
        // 外周はOuterCellとする
        case (0, _) => ((0, col), CellState.Outer)
        case (9, _) => ((9, col), CellState.Outer)
        case (_, 0) => ((row, 0), CellState.Outer)
        case (_, 9) => ((row, 9), CellState.Outer)
        // 中央4つの石を初期配置する
        case (4, 4) => ((4, 4), CellState.Light)
        case (4, 5) => ((4, 5), CellState.Dark)
        case (5, 4) => ((5, 4), CellState.Dark)
        case (5, 5) => ((5, 5), CellState.Light)
        // 内部をBlankに
        case _ => ((row, col), CellState.Blank)
      }
    }): _*)

  // 盤面の範囲内か確認
  def isInRange(row: Int, col: Int): Boolean = !(row < 1 || 8 < row || col < 1 || 8 < col)
}

case class Board(cells: SortedMap[(Int, Int), CellState] = Board.initialize()) {

  def setCell(row: Int, col: Int, cellState: CellState): Board = copy(cells = cells + ((row, col) -> cellState))

  def hasBlankCell: Boolean = cells.values.exists(_ == CellState.Blank)

  def countDarkDisks: Int = cells.values.count(_ == CellState.Dark)

  def countLightDisks: Int = 64 - countDarkDisks

  def flip(poss: Seq[CellPos]): Board = copy(cells = poss.foldLeft(cells){ (acc, pos) =>
    acc + ((pos.row, pos.col) -> acc(pos.row, pos.col).reverseDisk)
  })

  // 石の探索方向
  private val searchDirections: Seq[CellPos] = {for(i <- -1 to 1; j <- -1 to 1) yield CellPos(i, j)}
    .filter(_ != CellPos(0, 0))

  /**
    * 位置(row, col)にdiskで指定する色の石を置く時に反転する石を検索します。
    *
    * @param pos    石を置く位置
    * @param myDisk 置こうとする石
    * @return 反転する石のリスト
    */
  def selectFlippableCells(pos: CellPos, myDisk: CellState): Seq[CellPos] = {
    /**
      * 指定の1方向について反転可能な石を探索する
      *
      * @param acc アキュムレータ
      * @param currentColor 自分の石のカラー
      * @param reverseColor 反対の石のカラー
      * @param pos 探索開始位置
      * @param dir 探索方向
      * @return 探索結果のリスト。反転可能な石がなければサイズ0のリスト。
      */
    def scanCells(acc: Seq[CellPos], currentColor: CellState,
                          reverseColor: CellState, pos: CellPos, dir: CellPos): Seq[CellPos] = {
      cells(pos.row, pos.col) match {
        case `reverseColor` => scanCells(pos +: acc, currentColor, reverseColor, pos + dir, dir)
        case `currentColor` => acc
        case _ => Nil
      }
    }

    searchDirections.foldLeft(Seq[CellPos]()) {
      (acc, dir) => scanCells(Nil, myDisk, myDisk.reverseDisk, pos + dir, dir) ++ acc
    }
  }
}
