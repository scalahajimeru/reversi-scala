package othello

import othello.controller.GameController

object Main extends App {
  val c = new GameController("othello_gamestate.txt")
  c.run()
}
