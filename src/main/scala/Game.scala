import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.util.Random

case class Game(p1: Player, p2: Player, spaces: Queue[Pos], turn: PlayerID) {
  import Game._

  def isFinished: Option[Player] = {
    if p1.isWinner then Some(p1)
    else if p2.isWinner then Some(p2)
    else if spaces.isEmpty then Some(Ties)
    else None
  }

  def next: Game = {
    val (position: Pos, newSpaces: Queue[Pos]) = spaces.dequeue

    turn match {
      case PlayerID.P1 => Game(
        p1.copy(tokens = p1.tokens :+ position),
        p2,
        newSpaces,
        turn.next
      )
      case PlayerID.P2 => Game(
        p1,
        p2.copy(tokens = p2.tokens :+ position),
        newSpaces,
        turn.next
      )
      case invalid => throw new IllegalStateException(s"$invalid is an illegal player ID")
    }
  }
}

object Game {
  def init(): Game = {
    val p1 = Player(PlayerID.P1)
    val p2 = Player(PlayerID.P2)

    val spaces = Random.shuffle {
      Queue.from {
        for {
          x <- (0 until 3)
          y <- (0 until 3)
        } yield (x, y)
      }
    }

    Game(p1, p2, spaces, PlayerID.P1)
  }

  private val Ties = Player(PlayerID.Tie)

  @tailrec
  def play(game: Game, stats: Stats): Stats = game.isFinished match {
    case Some(Player(PlayerID.Tie, _)) => stats.copy(ties = stats.ties + 1, nGames = stats.nGames + 1)
    case Some(Player(PlayerID.P1, _)) => stats.copy(p1Wins = stats.p1Wins + 1, nGames = stats.nGames + 1)
    case Some(Player(PlayerID.P2, _)) => stats.copy(p2Wins = stats.p2Wins + 1, nGames = stats.nGames + 1)
    case Some(_) => throw new IllegalStateException("Illegal state during iteration.")
    case None => play(game.next, stats)
  }

}
