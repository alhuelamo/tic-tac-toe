package alhuelamo.tictactoe

case class Stats(p1Wins: Int, p2Wins: Int, ties: Int, nGames: Int)

object Stats {
  def empty: Stats = Stats(0, 0, 0, 0)
}
