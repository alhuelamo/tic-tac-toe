package alhuelamo.tictactoe

case class Player(name: PlayerID, tokens: Seq[Pos]) {
  def isEmpty: Boolean = tokens.isEmpty
  def iterator: Iterator[Pos] = tokens.iterator
  def size: Int = tokens.size

  def isWinner: Boolean =
    tokens.toSeq
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

object Player {
  def apply(name: PlayerID) = new Player(name, Seq.empty)
}

enum PlayerID {
  case P1, P2, Tie

  def next: PlayerID = this match {
    case P1 => P2
    case P2 => P1
    case Tie => throw new IllegalStateException("Tie is not part of the game!")
  }
}
