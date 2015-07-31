Words With Cheaters
===================

While playing Word with Friends, I became curious. What word would
actually cause the score meter's graph to actually hit its peak?
Intuitively, it seemed the graph was non-linear -- playing a word that
was just short of the maximum might still leave a lot of points on the
table. Was I just not seeing the best play or was the best play always
some out-of-the-ordinary word, like kabeljou[1].

By the way, playing the highest scoring word on every turn is not
necessarily the best way to win a game. It's apt to leave easy
openings for your opponent to use multiplers to good effect. That
said, it works pretty well against Zynga's computer players.

[1] an Afrikaans-derived name for a fish, known in Britain as the meagre.

Usage
-----

Install sbt, a la `brew install sbt`.

In the main directory, run:

  $ sbt "run -wwf -board=empty.wwf -hand=xypdqab -interactive"

This will load a board from the file empty.wwf (which, as you'd
expect, is an empty board) and produces a list of plays for the given
hand and some options.

Drop the `-interactive` flag to just get a list of the top 25 plays.

You can specify a blank tile by passing an asterisk (*) in the hand.

When loading the board, the program removes played letters from a list
of available tiles (a.k.a. the tile bag). An invalid board (e.g., one
where more E tiles have been played than exist in the game) will cause
the program to abort with an exception. Similarly, if you simulate
playing a hand that contains tiles that cannot still be in a rack,
you'll get an error.


Interactive Mode
----------------

In interactive mode, you'll be presented with the top 10 plays. You
can step through more plays by entering N for next or P for
previous. You can filter for plays that have a tile on a particular
row/column with F=r,c (rows and columns are indexed from zero). You
can see what the play looks like on the board with D=n (where n is one
of the enumerated plays). Finally, if you just choose a play by
entering its number the board file will be re-written with your play.

Board Files
-----------

The board file is trivial. Each line is a row, and each character in
the row is a column. Underscores represent unplayed squares. Upper
case letters represent played letters. Lower case letters represent
blank tiles that have been played and are now fixed as the given
letter.

The Algorithm
-------------

The algorithm started as an exhaustive, recursive search of the
possible placements of tiles: compute the places a tile may be placed
on the board, try every rack tile in each place. For each tile placed,
compute a new set of playable locations and try every remaining
tile. As each tile is placed, determine if the play is valid (all
newly formed words are in the dictionary) and remember the valid
plays.

The rules of the game allow some optimizations. During the recursion,
once a second tile has been placed, all subsequent tiles must be
placed in line with the first two, either horizontally or
vertically. I call this the direction of play. One optimization is
that if any word formed perpendicular to the direction of play is not
in the dictionary, the algorithm can short-circuit. Further, once a
particular perpendicular word has been validated it need not be
checked again.

Another optimization involved adding a bloom filter containing every
fragment of every word in the dictionary. The bloom filter takes the
current word (in the direction of play) and tells us whether, through
the addition of more letters (suffixes and/or prefixes) we can ever
produce a valid word.

Once all the valid plays are computed, they are scored and sorted.
