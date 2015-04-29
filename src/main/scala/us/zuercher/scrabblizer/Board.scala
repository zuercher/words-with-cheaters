package us.zuercher.scrabblizer

case class Location(row: Int, col: Int) {
  def validFor(board: Board) = row >= 0 && col >= 0 && row < board.size && col < board.size
}

sealed trait Square {
  def letterMultiplier: Int = 1
  def wordMultiplier: Int = 1
}

case object Normal extends Square
case object DoubleLetter extends Square { override val letterMultiplier = 2 }
case object TripleLetter extends Square { override val letterMultiplier = 3 }
case object DoubleWord extends Square { override val wordMultiplier = 2 }
case object TripleWord extends Square { override val wordMultiplier = 3 }

case class Value(square: Square, letterValue: Int)

object Board {
  def mirror(size: Int, locations: Seq[Location]): Seq[Location] = {
    val all =
      locations ++ locations.map {
        case Location(r, c) => Location(size - r - 1, c)
      } ++ locations.map {
        case Location(r, c) => Location(r, size - c - 1)
      } ++ locations.map {
        case Location(r, c) => Location(size - r - 1, size - c - 1)
      }
    all.distinct
  }
}

class Board(
  val size: Int,
  val initialTileBag: Map[Char, Int],
  val letterValues: Map[Char, Int],
  val doubleLetterScores: Seq[Location],
  val tripleLetterScores: Seq[Location],
  val doubleWordScores: Seq[Location],
  val tripleWordScores: Seq[Location],
  val bingo: Int,
  val dictionary: Dictionary)
{
  private[Board] val board: Map[Location, Square] = {
    val m = Map.empty[Location, Square].withDefaultValue(Normal)

    Seq(
      DoubleLetter -> doubleLetterScores,
      TripleLetter -> tripleLetterScores,
      DoubleWord   -> doubleWordScores,
      TripleWord   -> tripleWordScores
    ).foldLeft(m) { (m, squareData) =>
      val (square, locations) = squareData
      locations.foldLeft(m) { (m, location) =>
        m + (location -> square)
      }
    }
  }

  override def equals(other: Any): Boolean = {
    other match {
      case that: Board =>
        (this eq that) || (
          (this.size == that.size) &&
            (this.board == that.board) &&
            (this.initialTileBag == that.initialTileBag) &&
            (this.doubleLetterScores == that.doubleLetterScores) &&
            (this.tripleLetterScores == that.tripleLetterScores) &&
            (this.doubleWordScores == that.doubleWordScores) &&
            (this.tripleWordScores == that.tripleWordScores)
        )
      case _ => false
    }
  }

  def value(location: Location, letter: Char, isPlay: Boolean): Value = {
    val letterValue = letterValues.getOrElse(letter, 0)
    if (isPlay) {
      Value(board(location), letterValue)
    } else {
      Value(Normal, letterValue)
    }
  }

  def score(word: Seq[Value]): Int = {
    val base =
      word.foldLeft(0) { (score, value) =>
        score + (value.letterValue * value.square.letterMultiplier)
      }

    val wordMultiplier = word.foldLeft(1) { (mult, value) => mult * value.square.wordMultiplier }

    base * wordMultiplier
  }

  override def toString() = {
    import Colorize.withColor

    (0 until size).map { row =>
      (0 until size).map { col =>
        board(Location(row, col)) match {
          case Normal => "_"
          case DoubleLetter => withColor(Color.Blue)("d")
          case TripleLetter => withColor(Color.Green)("t")
          case DoubleWord   => withColor(Color.Red)("D")
          case TripleWord   => withColor(Color.Yellow)("T")
        }
      }.mkString("")
    }.mkString("\n")
  }
}
