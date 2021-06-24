import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.util.Random

@main def simulate: Unit = {
  val finalStats = (0 until 1000000)
    .foldLeft(Stats.empty) { (accStats, _) =>
      Game.play(Game.init(), accStats)
    }
  println(finalStats)
}

type Pos = (Int, Int)
case class Player(name: String, tokens: Seq[Pos]) {
  def isEmpty: Boolean = tokens.isEmpty
  def iterator: Iterator[Pos] = tokens.iterator
  def size: Int = tokens.size
}

object Player {
  def apply(name: String) = new Player(name, Seq.empty)
}

case class Game(p1: Player, p2: Player, spaces: Queue[Pos], turn: Game.Turn) {
  import Game._

  def isFinished: Option[Player] = {
    if isWinner(p1) then Some(p1)
    else if isWinner(p2) then Some(p2)
    else if spaces.isEmpty then Some(Ties)
    else None
  }

  def next: Game = {
    val (position: Pos, newSpaces: Queue[Pos]) = spaces.dequeue

    turn match {
      case Turn.P1 => Game(
        p1.copy(tokens = p1.tokens :+ position),
        p2,
        newSpaces,
        turn.next
      )
      case Turn.P2 => Game(
        p1,
        p2.copy(tokens = p2.tokens :+ position),
        newSpaces,
        turn.next
      )
    }
  }
}

object Game {
  def init(): Game = {
    val p1 = Player("p1")
    val p2 = Player("p2")

    val spaces = Random.shuffle {
      Queue.from {
        for {
          x <- (0 until 3)
          y <- (0 until 3)
        } yield (x, y)
      }
    }

    new Game(p1, p2, spaces, Turn.P1)
  }

  private val Ties = Player("ties")

  @tailrec
  def play(game: Game, stats: Stats): Stats = game.isFinished match {
    case Some(Player(Ties.name, _)) => stats.copy(ties = stats.ties + 1, nGames = stats.nGames + 1)
    case Some(Player("p1", _)) => stats.copy(p1Wins = stats.p1Wins + 1, nGames = stats.nGames + 1)
    case Some(Player("p2", _)) => stats.copy(p2Wins = stats.p2Wins + 1, nGames = stats.nGames + 1)
    case Some(_) => throw new IllegalStateException("Illegal state during iteration.")
    case None => play(game.next, stats)
  }

  sealed trait Turn {
    import Turn._

    def next: Turn = this match {
      case P1 => P2
      case P2 => P1
    }
  }
  object Turn {
    case object P1 extends Turn
    case object P2 extends Turn
  }
}

case class Stats(p1Wins: Int, p2Wins: Int, ties: Int, nGames: Int)

object Stats {
  def empty: Stats = Stats(0, 0, 0, 0)
}

def isWinner(player: Player): Boolean = {
  player.tokens.toSeq
    .combinations(3)
    .filter(_.length >= 3)
    .map { combination =>
      combination
        .reduce { case ((x1, y1), (x2, y2)) =>
          ((x1 + x2), (y1 + y2))
        }
    }
    .exists { case (sumX, sumY) =>
      val result = (sumX % 3 == 0) && (sumY % 3 == 0)
      result
    }
}
