package us.zuercher.scrabblizer

import java.io.{File, FileInputStream}
import scala.collection.mutable

case class InvalidBoardDataException(msg: String) extends Exception(msg)

sealed trait Validity
case object Valid extends Validity
case object MayBecomeValid extends Validity
case class Invalid(reason: String) extends Validity

object Position {
  def parse(board: Board, file: File): Position = {
    val iter = new LineIterator(new FileInputStream(file))
    val buffer = new StringBuffer
    while(iter.hasNext) {
      buffer.append(iter.next())
      buffer.append('\n')
    }
    parse(board, buffer.toString)
  }

  def parse(board: Board, data: String): Position = {
    val position = empty(board)

    val rows = data.split("\n").toSeq
    if (rows.size != position.size) {
      throw InvalidBoardDataException(
        "expected %d rows, got %d rows".format(position.size, rows.size)
      )
    }

    rows.zipWithIndex.foldLeft(position) { (position, rowData) =>
      val (row, r) = rowData
      if (row.size != position.size) {
        throw InvalidBoardDataException(
          "expected %d columns in row %d, got %d columns".format(position.size, r, row.size)
        )
      }

      row.zipWithIndex.foldLeft(position) { (position, colData) =>
        val (letter, c) = colData
        if (Character.isLetter(letter) && Character.toUpperCase(letter) <= 'Z') {
          if (Character.isLowerCase(letter)) {
            position.set(Location(r, c), Existing(Character.toUpperCase(letter), isBlank = true))
          } else {
            position.set(Location(r, c), Existing(Character.toUpperCase(letter)))
          }
        } else {
          position
        }
      }
    }
  }

  def empty(board: Board): Position = new Position(board)
}

sealed trait Tile {
  def char: Char = '?'
  def isBlank = false
  def isPlay = false
  def tileBagChar = if (isBlank) ' ' else char
}

case object Empty extends Tile
case class Existing(override val char: Char, override val isBlank: Boolean = false) extends Tile
case class Check(override val char: Char, override val isBlank: Boolean = false) extends Tile {
  override val isPlay = true
}

class Position private[Position](
    val board: Board,
    val position: Map[Location, Tile],
    val tileBag: Map[Char, Int])
{
  def this(board: Board) =
    this(
      board,
      Map.empty[Location, Tile].withDefaultValue(Empty),
      board.initialTileBag
    )

  override def equals(other: Any) = {
    other match {
      case that: Position => (this.board == that.board) && (this.position == that.position)
      case _ => false
    }
  }

  override def hashCode = {
    board.hashCode ^ position.hashCode
  }

  def size = board.size

  def set(location: Location, tile: Tile): Position = {
    require(location.validFor(board), s"invalid location: $location")
    require(position(location) == Empty, s"cannot play on an existing location: $location")
    require(tile != Empty, "cannot play an empty tile")
    require(
      tileBag.get(tile.tileBagChar) exists { _ > 0 },
      s"tile bag does not contain a(n) '${tile.tileBagChar}'"
    )

    new Position(
      board,
      position + (location -> tile),
      tileBag + (tile.tileBagChar -> (tileBag(tile.tileBagChar) - 1))
    )
  }

  def availableTiles: Set[Char] = tileBag.keys.toSet

  def play(location: Location, letter: Char, isBlank: Boolean = false): Position =
    set(location, Check(Character.toUpperCase(letter), isBlank))

  def valid: Validity = {
    // must not have over-used available tiles
    if (tileBag.values exists { _ < 0 }) return Invalid("over-used available tiles")

    // all check tiles must be in a single row or column
    if (playedLocations.isEmpty) return Valid
    if (isPlayHorizontal && isPlayVertical) return Invalid("simultaneous horizontal & verical play")

    // word must connect to existing tiles (or else be first play on center of board)
    if (isFirstPlay) {
      if (playedLocations(middle) == Empty) return Invalid("first tile not in middle")
    } else {
      // no blank spaces in the played direction
      val words = playedLocations map { case (location, _) => findMainWord(location) }
      if (words.toSet.size != 1) return Invalid("multiple words in played direction")

      val mainLocations = findMainLocations(playedLocations.keys.head)
      val mainHasExisting = mainLocations.exists { location =>
        position(location) match {
          case Existing(_, _) => true
          case _ => false
        }
      }

      if (!mainHasExisting) {
        // check perpendiculars
        val perpendicularHasExisting =
          playedLocations.exists { case (location, _) =>
            val perpendicularLocations = findPerpendicularLocations(location)
            perpendicularLocations.exists { location =>
              position(location) match {
                case Existing(_, _) => true
                case _ => false
              }
            }
          }

        if (!perpendicularHasExisting) return Invalid("play not adjacent to previously played tiles")
      }
    }

    // all newly formed words must be in the dictionary
    checkWords
  }

  val playedLocations: Map[Location, Tile] =
    position.filter {
      case (_, Check(_, _)) => true
      case _ => false
    }

  val isFirstPlay = playedLocations.size == position.size

  val isPlayVertical: Boolean = {
    playedLocations.map { case (Location(r, _), _) => r }.toSet.size > 1
  }

  val isPlayStrictlyHorizontal: Boolean = {
    playedLocations.map { case (Location(_, c), _) => c }.toSet.size > 1
  }
  val isPlayHorizontal: Boolean = {
    // consider single-letter plays horizontal
   playedLocations.size == 1 || isPlayStrictlyHorizontal
  }

  // presumes a valid board (particularly no gaps in words)
  def checkWords: Validity = {
    val mainWord = findMainWord(playedLocations.keys.head)
    val mainOk = mainWord.exists { board.dictionary.contains(_) }

    val perpendicularsOk =
      playedLocations.forall { case (location, _) =>
          val perpendicularWord = findPerpendicularWord(location)
          perpendicularWord.forall { board.dictionary.contains(_) }
      }

    if (!perpendicularsOk) return Invalid("word(s) perpendicular to play are not in dictionary")
    if (mainOk) return Valid

    // mainWord is None on first played tile
    if (mainWord.forall { board.dictionary.mightExpandToWord(_) }) {
      MayBecomeValid
    } else {
      Invalid(s"main word ($mainWord) is not a fragment of any word in the dictionary")
    }
  }

  def findMainWord(location: Location): Option[String] = {
    if (isPlayHorizontal) {
      findHorizontalWord(location)
    } else {
      findVerticalWord(location)
    }
  }

  def findPerpendicularWord(location: Location): Option[String] = {
    if (isPlayHorizontal) {
      findVerticalWord(location)
    } else {
      findHorizontalWord(location)
    }
  }

  def findVerticalWord(location: Location): Option[String] = {
    val locations = findVerticalLocations(location)
    if (locations.isEmpty) {
      None
    } else {
      Some(locations.map { position(_).char }.mkString)
    }
  }

  def findHorizontalWord(location: Location): Option[String] = {
    val locations = findHorizontalLocations(location)
    if (locations.isEmpty) {
      None
    } else {
      Some(locations.map { position(_).char }.mkString)
    }
  }

  def findMainLocations(location: Location): Seq[Location] = {
    if (isPlayHorizontal) {
      findHorizontalLocations(location)
    } else {
      findVerticalLocations(location)
    }
  }

  def findPerpendicularLocations(location: Location): Seq[Location] = {
    if (isPlayHorizontal) {
      findVerticalLocations(location)
    } else {
      findHorizontalLocations(location)
    }
  }

  def findVerticalLocations(location: Location): Seq[Location] = {
    if (position(location) == Empty) return Seq.empty

    val aboveRows = (location.row - 1) to 0 by -1
    val above = aboveRows.map { r => location.copy(row = r) }.takeWhile { position(_) != Empty }

    val belowRows = (location.row + 1) until board.size
    val below = belowRows.map { r => location.copy(row = r) }.takeWhile { position(_) != Empty }

    if (above.isEmpty && below.isEmpty) {
      Seq.empty
    } else {
      above.reverse ++ Seq(location) ++ below
    }
  }

  def findHorizontalLocations(location: Location): Seq[Location] = {
    if (position(location) == Empty) return Seq.empty

    val leftCols = (location.col - 1) to 0 by -1
    val left = leftCols.map { c => location.copy(col = c) }.takeWhile { position(_) != Empty }

    val rightCols = (location.col + 1) until board.size
    val right = rightCols.map { c => location.copy(col = c) }.takeWhile { position(_) != Empty }

    if (left.isEmpty && right.isEmpty) {
      Seq.empty
    } else {
      left.reverse ++ Seq(location) ++ right
    }
  }

  def score: Int = {
    if (valid != Valid) return 0

    val mainWordLocations = findMainLocations(playedLocations.keys.head)

    val values =
      mainWordLocations map { location =>
        val tile = position(location)
        board.value(location, tile.tileBagChar, tile.isPlay)
      }
    val mainWordScore = board.score(values)

    val wordScore =
      playedLocations.foldLeft(mainWordScore) { (score, playData) =>
        val (location, _) = playData
        val perpendicularWordLocations = findPerpendicularLocations(location)

        val values = perpendicularWordLocations map { location =>
          val tile = position(location)
          board.value(location, tile.tileBagChar, tile.isPlay)
        }
        score + board.score(values)
      }

    if (playedLocations.size == 7) {
      wordScore + board.bingo
    } else {
      wordScore
    }
  }

  def middle = Location(board.size / 2, board.size / 2)

  // Returns locations where letters may be played (without regard to whether any letter
  // in the location could ever spell a word in the dictionary).
  lazy val playableLocations: Seq[Location] = {
    def adjacents(loc: Location): Seq[Location] = {
      Seq(
        Location(loc.row - 1, loc.col),
        Location(loc.row + 1, loc.col),
        Location(loc.row, loc.col - 1),
        Location(loc.row, loc.col + 1)
      ) filter { location =>
        location.validFor(board) && position(location) == Empty
      }
    }

    if (position.isEmpty) {
      Seq(middle)
    } else if (playedLocations.isEmpty) {
      // can play off any existing
      position.keys.flatMap(adjacents).toSeq
    } else if (playedLocations.size == 1) {
      // can play horizontally or vertically from the playedLocation
      adjacents(playedLocations.keys.head)
    } else if (isPlayStrictlyHorizontal) {
      // can continue this play horizontally
      val locations = findHorizontalLocations(playedLocations.keys.head)
      val lefter =
        locations.headOption map {
          location => location.copy(col = location.col - 1)
        } filter { _.validFor(board) }
      val righter =
        locations.lastOption map {
          location => location.copy(col = location.col + 1)
        } filter { _.validFor(board) }

      (lefter ++ righter).toSeq
    } else {
      // can continue this play vertically
      val locations = findVerticalLocations(playedLocations.keys.head)
      val higher =
        locations.headOption map {
          location => location.copy(row = location.row - 1)
        } filter { _.validFor(board) }
      val lower =
        locations.lastOption map {
          location => location.copy(row = location.row + 1)
        } filter { _.validFor(board) }
      (higher ++ lower).toSeq
    }
  }

  override def toString() = {
    (0 until board.size).map { r =>
      (0 until board.size).map { c =>
        position(Location(r, c)) match {
          case Empty => '_'
          case Existing(ch, true) => Character.toLowerCase(ch) // TODO represent blank
          case Existing(ch, false) => Character.toLowerCase(ch)
          case Check(ch, true) => Character.toUpperCase(ch)
          case Check(ch, false) => Character.toUpperCase(ch)
        }
      }.mkString
    }.mkString("\n")
  }
}
