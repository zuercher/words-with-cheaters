package us.zuercher.scrabblizer

import com.twitter.app.{App, Flag}
import com.twitter.util.Stopwatch
import java.io.File

object Main extends App {
  val findFlag = flag[String]("find", "search for a word")

  val wordsWithFriendsFlag = flag("wwf", false, "enables words with friends solver")

  val boardFlag = flag[String]("board", "path to a board file")
  val handFlag = flag[String]("hand", "hand to use for solving a board")

  def main() {
    findFlag.get match {
      case Some(searchTerm) => return find(searchTerm)
      case None => ()
    }

    (wordsWithFriendsFlag.get, boardFlag.get, handFlag.get) match {
      case (Some(true), Some(board), Some(hand)) =>
        return solveWordsWithFriends(new File(board), hand)
      case _ => ()
    }

    println(flag.usage)
  }

  def find(searchTerm: String) {
    val matches = Dictionary.default.matching(searchTerm).all

    println(matches.mkString("\n"))
  }

  def solveWordsWithFriends(boardFile: File, hand: String) {
    val stopwatch = Stopwatch.start()

    val startingPosition = Position.parse(WordsWithFriends.board, boardFile)

    val parsedHand = hand.toUpperCase.replace('*', ' ')

    val plays = Solver(startingPosition, parsedHand)
    plays foreach { case (Play(word, location, orientation, score), position) =>
      println(
        "%7s r%2d, c%2d %10s %3d".format(
          word,
          location.row,
          location.col,
          orientation.toString.toLowerCase,
          score
        )
      )
    }

    plays.headOption foreach { case (Play(word, location, orientation, score), position) =>
        println("Best:")
        println(
          "%7s r%2d, c%2d %10s %3d".format(
            word,
            location.row,
            location.col,
            orientation.toString.toLowerCase,
            score
          )
        )
        println(position)
    }

    val elapsed = stopwatch()
    println("Runtime: %s".format(elapsed))
  }
}
