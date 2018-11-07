package othello.controller

import java.io.IOException

import othello.model.Board.CellsOps
import othello.model.GameState._
import othello.model._
import othello.view.GameView

import scala.io.StdIn

class GameController(gamestateFileName: String) {

  def run(): Unit = {
    def mainLoop(state: GameState): Unit = {
      print("Othello>")
      val line: String = try {
        StdIn.readLine
      } catch {
        case e: IOException =>
          e.printStackTrace()
          ""
      }

      // コマンド解釈
      val tokens = line.split("\\s+")
      if (tokens.nonEmpty) {
        tokens(0) match {
          case "help" => commandHelp(tokens); mainLoop(state)
          case "board" | "b" => commandBoard(tokens, state); mainLoop(state)
          case "put" | "p" => mainLoop(commandPut(tokens, state))
          case "pass" => mainLoop(commandPass(tokens, state))
          case "reset" => mainLoop(commandReset(tokens, state))
          case "" => mainLoop(state)
          case "exit" => Unit
          case _ => println("無効なコマンドです。"); mainLoop(state)
        }
      }
    }

    mainLoop(GameState())
  }

  private def commandHelp(tokens: Array[String]): Unit = {
    println("Usage:")
    println("  help    このメッセージを表示します。")
    println("  board   盤面の状態を表示します。")
    println("  put [行] [列]")
    println("          盤面の指定の位置に石を置きます。")
    println("          例) put 2 d")
    println("              2行d列のマスに石を配置する")
    println("  pass    パスします。(次の人に手を譲る)")
    println("  reset   盤面を初期化します。")
  }

  private def commandBoard(tokens: Array[String], state: GameState): Unit = GameView.showState(state)

  private def commandPut(tokens: Array[String], state: GameState): GameState = {
    def readParams(tokens: Array[String]): Either[String, (Int, Int)] = tokens match {
      case Array(_, token1, token2) =>

        // 引数1の取り出し
        val arg1opt: Either[String, Int] = try {
          Right(token1.toInt)
        } catch {
          case _: NumberFormatException =>
            Left("Invalid argument: " + token1 + "\n数値を入力してください。")
        }

        // 引数2の取り出し
        val a_h = "([a-h])".r
        val arg2opt: Either[String, Int] = token2 match {
          case a_h(s: String) =>
            Right(s(0) - 'a' + 1)
          case _ =>
            Left("a～hの文字を入力してください。")
        }

        for (row <- arg1opt; col <- arg2opt) yield {
          (row, col)
        }
      case _ =>
        Left("Invalid argument count.")
    }

    // 盤面の範囲内か確認
    def isInRange(row: Int, col: Int): Either[String, Unit] = if (Board.isInRange(row, col))
      Right(Unit)
    else
      Left("盤面の範囲外です。")

    // 既に置いてあるか確認
    def isBlank(row: Int, col: Int): Either[String, Unit] = if (state.cells.exists {
      case ((`row`, `col`), CellState.Blank) => true
      case _ => false
    }) Right(Unit) else Left("既に石が置かれています。")

    val disk = turnToState(state.turn)

    // 反転できるか探索する
    // 全8方向について、異なる色の石がある間、この石をカウントしながら進み、次の条件
    // ・盤の範囲外となる(OuterCell)
    // ・空のマスとなる(BlankCell)
    // ・同じ色の石となる(DarkDisk|LightDisk)
    // に合致すればループを終了する。
    // 同じ色の石が見つかったときのみ、異なる色の石を反転させる。
    def selectFlippableCells(row: Int, col: Int, cellState: CellState): Either[String, Seq[CellPos]] = {
      val flipCells = state.cells.selectFlippableCells(CellPos(row, col), disk)

      // 反転する石があるか？
      if (flipCells.nonEmpty) {
        Right(flipCells)
      } else {
        // 反転する石がない
        Left("反転できる石がありません。")
      }
    }

    def doFlip(row: Int, col: Int, flipCells: Seq[CellPos]): GameState = {
      val flippedCells = state.cells
        .setCell(row, col, disk) // 石を置いて
        .flip(flipCells) // ひっくり返す
      state
        .copy(
          cells = flippedCells,
          moveCount = state.moveCount + 1)
    }

    def evalGameContinue(state: GameState): GameState = {
      // ゲーム終了か調べる
      val state2 = if (state.cells.hasBlankCell) { // BlankCellがあればゲーム継続
        state.switchTurn()
      } else { // BlankCellがなければゲーム終了
        println((if (state.cells.countDarkDisks > 32) "黒"
        else "白") + "の勝ち")
        state.copy(gameover = true)
      }

      GameView.showState(state2)
      state2
    }

    (for {
      rowcol <- readParams(tokens)
      _ <- isInRange(rowcol._1, rowcol._2)
      _ <- isBlank(rowcol._1, rowcol._2)
      flipCells <- selectFlippableCells(rowcol._1, rowcol._2, disk)
    } yield {
      evalGameContinue(doFlip(rowcol._1, rowcol._2, flipCells))
    }) match {
      case Right(newState) => newState
      case Left(message) => println(message); state
    }

  }

  private def commandPass(tokens: Array[String], state: GameState): GameState = {
    val newState = state.pass()
    GameView.showState(newState)
    newState
  }

  private def commandReset(tokens: Array[String], state: GameState): GameState = {
    state.initialize()
  }

}
