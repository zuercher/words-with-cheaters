package us.zuercher.scrabblizer

import java.io.{BufferedReader, FileInputStream, InputStream, InputStreamReader}

class LineIterator(inputStream: InputStream) extends Iterator[String] {
  private[this] val reader = new BufferedReader(new InputStreamReader(inputStream, "UTF-8"))
  private[this] var nextLine: Option[String] = None

  private[this] def readNext() {
    nextLine = Option(reader.readLine())
    if (nextLine.isEmpty) {
      reader.close()
    }
  }

  override def hasNext = nextLine.isDefined
  override def next() = {
    nextLine match {
      case Some(s) =>
        readNext()
        s
      case None => throw new NoSuchElementException()
    }
  }

  readNext()
}
