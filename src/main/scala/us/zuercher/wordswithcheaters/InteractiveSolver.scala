package us.zuercher.wordswithcheaters

import java.io.File
import scala.annotation.tailrec

case class Filter(location: Location) {
  def matches(position: Position): Boolean = position.containsPlayedLocation(location)
}

sealed trait Choice
case class Quit(message: String) extends Choice
case class Pick(index: Int) extends Choice
case class ListPlays(start: Int, filter: Option[Filter] = None) extends Choice
case class DisplayPlay(index: Int) extends Choice
case object UnknownCommand extends Choice

object InteractiveSolver {
  def apply(board: Board, output: File, plays: Seq[(Play, Position)], limit: Int = 10) =
    new InteractiveSolver(board, output, plays, limit)
}

class InteractiveSolver(
    board: Board,
    output: File,
    originalPlays: Seq[(Play, Position)],
    limit: Int)
{
  val PickRegex = "^([0-9]+)$".r
  val DisplayRegex = "^D=([0-9]+)$".r
  val FilterRegex = "^F=([0-9]+),([0-9]+)$".r

  val allPlays = originalPlays.zipWithIndex map { case ((play, pos), index) => (index, play, pos) }

  def apply(start: Int = 0, filter: Option[Filter] = None) {
    run(allPlays, start, filter)
  }

  @tailrec
  private[this] def run(plays: Seq[(Int, Play, Position)], start: Int, filter: Option[Filter]) {
    val shown = plays.drop(start).take(limit)
    shown foreach { case (index, play, _) =>
      println("%4d) ".format(index) + play.toFormattedString)
    }
    println("Choose play (#), (N)ext group, (P)rev group, (Q)uit")
    println("       display (D=#), filter (F=r,c), (C)lear filter")

    val input = Option(readLine()).map { _.toUpperCase }
    val choice =
      input match {
        case Some(PickRegex(str)) =>
          val choice = str.toInt
          if (choice >= allPlays.length) {
            ListPlays(start, filter)
          } else {
            Pick(choice)
          }

        case Some("N") => ListPlays((start + limit) min (allPlays.length - limit), filter)

        case Some("P") => ListPlays((start - limit) max 0, filter)

        case Some(DisplayRegex(str)) =>
          val index = str.toInt
          if (index >= allPlays.length) {
            ListPlays(start, filter)
          } else {
            DisplayPlay(index)
          }

        case Some(FilterRegex(row, col)) =>
          val location = Location(row.toInt, col.toInt)
          if (location.validFor(board)) {
            ListPlays(0, Some(Filter(location)))
          } else {
            UnknownCommand
          }

        case Some("C") =>
          ListPlays(0)

        case Some("Q") | None => Quit("quitting")

        case _ => UnknownCommand
      }

    choice match {
      case Quit(message) =>
        println(message)

      case Pick(choice) =>
        val (_, _, position) = allPlays(choice)
        println(position)
        position.store(output)

      case UnknownCommand =>
        println(s"Unknown (or invalid) command: $input")
        run(plays, start, filter)

      case DisplayPlay(index) =>
        val (_, play, position) = allPlays(index)
        println(play.toFormattedString.trim)
        println(position)
        println()
        run(plays, start, filter)

      case ListPlays(newStart, newFilter) =>
        val updatedPlays =
          if (filter != newFilter) {
            newFilter match {
              case Some(filter) =>
                allPlays.filter { case (_, _, position) => filter.matches(position) }
              case None =>
                allPlays
            }
          } else {
            plays
          }
        run(updatedPlays, newStart, newFilter)
    }
  }
}
