package us.zuercher.wordswithcheaters

import com.twitter.app.{App, Flag}
import com.twitter.util.Stopwatch
import java.io.File

object Main extends App {
  val findFlag = flag[String]("find", "search for a word")

  val wordsWithFriendsFlag = flag("wwf", false, "enables words with friends solver")

  val boardFlag = flag[String]("board", "path to a board file")
  val handFlag = flag[String]("hand", "hand to use for solving a board")

  val interactive = flag("interactive", false, "enables interactive mode")

  def main() {
    findFlag.get match {
      case Some(searchTerm) => return find(searchTerm)
      case None => ()
    }

    (wordsWithFriendsFlag(), boardFlag.get, handFlag.get) match {
      case (true, Some(board), Some(hand)) =>
        return solveWordsWithFriends(new File(board), hand, interactive())
      case _ => ()
    }

    println(flag.usage)
  }

  def find(searchTerm: String) {
    val matches = Dictionary.default.matching(searchTerm).all

    println(matches.mkString("\n"))
  }

  def reportElapsed[T](desc: String)(f: => T): T = {
    val stopwatch = Stopwatch.start()
    val result = f
    val elapsed = stopwatch()
    println(s"$desc $elapsed")
    result
  }

  def dumpSolutions(plays: Seq[(Play, Position)], n: Int = 25, showBest: Boolean = true) {
    val topN = plays.take(n)
    println("Top %d:".format(topN.length))
    topN foreach { case (play, _) =>
      println(play.toFormattedString)
    }

    if (showBest) {
      plays.headOption foreach { case (play, position) =>
        println()
        println("Best:")
        println(play.toFormattedString)
        println(position)
      }
    }
  }

  def solveWordsWithFriends(boardFile: File, hand: String, interactive: Boolean) {
    val startingPosition = Position.parse(WordsWithFriends.board, boardFile)

    val parsedHand = hand.toUpperCase.replace('*', ' ')

    val plays =
      reportElapsed("solve time ") {
        Solver(startingPosition, parsedHand)
      }

    if (interactive) {
      InteractiveSolver(startingPosition.board, boardFile, plays).apply()
    } else {
      dumpSolutions(plays)
    }
  }
}
