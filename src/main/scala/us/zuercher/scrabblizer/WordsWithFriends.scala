package us.zuercher.scrabblizer

object WordsWithFriends  {
  import Board._

  val wordsWithFriendsTileBag =
    Map[Char, Int](
      ' ' -> 2,
      'A' -> 9,
      'B' -> 2,
      'C' -> 2,
      'D' -> 5,
      'E' -> 13,
      'F' -> 2,
      'G' -> 3,
      'H' -> 4,
      'I' -> 8,
      'J' -> 1,
      'K' -> 1,
      'L' -> 4,
      'M' -> 2,
      'N' -> 5,
      'O' -> 8,
      'P' -> 2,
      'Q' -> 1,
      'R' -> 6,
      'S' -> 5,
      'T' -> 7,
      'U' -> 4,
      'V' -> 2,
      'W' -> 2,
      'X' -> 1,
      'Y' -> 2,
      'Z' -> 1
    )

  val wordsWithFriendsLetterScores =
    Map[Char, Int](
      ' ' -> 0,
      'A' -> 1,
      'B' -> 4,
      'C' -> 4,
      'D' -> 2,
      'E' -> 1,
      'F' -> 4,
      'G' -> 3,
      'H' -> 3,
      'I' -> 1,
      'J' -> 10,
      'K' -> 5,
      'L' -> 2,
      'M' -> 4,
      'N' -> 2,
      'O' -> 1,
      'P' -> 4,
      'Q' -> 10,
      'R' -> 1,
      'S' -> 1,
      'T' -> 1,
      'U' -> 2,
      'V' -> 5,
      'W' -> 4,
      'X' -> 8,
      'Y' -> 3,
      'Z' -> 10
    )

  val size = 15

  lazy val board: Board =
    new Board(
      size,
      wordsWithFriendsTileBag,
      wordsWithFriendsLetterScores,
      doubleLetterScores =
        mirror(
          size,
          Seq(
            Location(1, 2),
            Location(2, 1),
            Location(2, 4),
            Location(4, 2),
            Location(4, 6),
            Location(6, 4)
          )
        ),
      tripleLetterScores =
        mirror(
          size,
          Seq(
            Location(0, 6),
            Location(3, 3),
            Location(5, 5),
            Location(6, 0)
          )
        ),
      doubleWordScores =
        mirror(
          size,
          Seq(
            Location(1, 5),
            Location(5, 1),
            Location(3, 7),
            Location(7, 3)
          )
        ),
      tripleWordScores =
          mirror(
          size,
          Seq(
            Location(0, 3),
            Location(3, 0)
          )
        ),
      bingo = 35,
      Dictionary.fromFile("resource:words_with_friends.txt")
    )
}
