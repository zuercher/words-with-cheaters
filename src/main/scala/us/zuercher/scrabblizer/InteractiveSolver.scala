package us.zuercher.scrabblizer

import java.io.File
import scala.annotation.tailrec

sealed trait Choice
case class Quit(message: String) extends Choice
case class Pick(index: Int) extends Choice
case class Show(start: Int) extends Choice

object InteractiveSolver {
  val Numeric = "^([0-9]+)$".r

  @tailrec
  def apply(output: File, plays: Seq[(Play, Position)], start: Int = 0, limit: Int = 10) {
    println(s"start: $start")
    val shown = plays.drop(start).take(limit)
    shown.zipWithIndex foreach { case ((play, _), index) =>
      println("%4d) ".format(start + index) + play.toFormattedString)
    }
    println("Choose play (#), (N)ext group, (P)rev group, (Q)uit")
    val input = Option(readLine()).map { _.toUpperCase }
    val choice =
      input match {
        case Some("Q") | None => Quit("quitting")
        case Some(Numeric(str)) => 
          val choice = str.toInt
          if (choice >= plays.length) {
            Show(start)
          }  else {
            Pick(choice)
	  }

      case Some("N") => 
        println(s"start: $start, limit $limit, plays.length ${plays.length}")
        Show((start + limit) min (plays.length - limit))
      case Some("P") => Show((start - limit) max 0)
      case _ => Show(start)
    }

    choice match {
      case Quit(message) => println(message)
      case Pick(choice) =>
        val (_, position) = plays(choice)
	position.store(output)
      case Show(newStart) => apply(output, plays, newStart, limit)
    }    
  }
}
