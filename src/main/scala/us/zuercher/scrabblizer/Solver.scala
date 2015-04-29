package us.zuercher.scrabblizer

import scala.collection.mutable.ListBuffer

sealed trait Orientation
case object Vertical extends Orientation
case object Horizontal extends Orientation

case class Play(word: String, location: Location, orientation: Orientation, score: Int)

object Solver {
  def apply(startingPosition: Position, handLetters: String): Seq[(Play, Position)] = {
    val letters = handLetters.toUpperCase.toSeq

    val hand = letters.groupBy(identity).map { case (letter, repeats) => letter -> repeats.size }

    require(
      hand.forall { case (letter, n) => startingPosition.tileBag.get(letter).exists { _ >= n } },
      s"hand ('$hand') has letters not currently in the position's tile bag"
    )

    val plays = ListBuffer.empty[(Play, Position)]

    solve(startingPosition, hand, plays)

    plays.sortBy { case (play, pos) => 0 - play.score }.toSeq.distinct
  }

  def solve(position: Position, hand: Map[Char, Int], plays: ListBuffer[(Play, Position)]) {
    position.playableLocations foreach { location =>
      hand foreach { case (letter, num) =>
        val nextHand =
          if (num > 1) {
            hand + (letter -> (num - 1))
          } else {
            hand - letter
          }

        if (letter == ' ') {
          'A' to 'Z' foreach { letter =>
            val newPosition = position.play(location, letter, isBlank = true)
            if (checkPosition(newPosition, location, plays)) {
              solve(newPosition, nextHand, plays)
            }
          }
        } else {
          val newPosition = position.play(location, letter)
          if (checkPosition(newPosition, location, plays)) {
            solve(newPosition, nextHand, plays)
          }
        }
      }
    }
  }

  private[this] def checkPosition(
    position: Position,
    location: Location,
    plays: ListBuffer[(Play, Position)]
  ): Boolean = {
    position.valid match {
      case Valid =>
        val word = position.findMainWord(location).get
        val start = position.findMainLocations(location).head
        val orientation = if (position.isPlayHorizontal) Horizontal else Vertical
        val play = Play(word, start, orientation, position.score)
        // println(s"...${plays.size}, ${play}")
        plays.append(play -> position)
        true
      case MayBecomeValid => true
      case Invalid(_) => false
    }
  }
}
