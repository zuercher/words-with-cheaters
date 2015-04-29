package us.zuercher.scrabblizer

import com.google.common.base.Charsets
import com.google.common.hash.{BloomFilter, Funnels}
import java.io._
import java.util.concurrent.atomic.AtomicReference
import scala.collection.immutable.SortedSet

case class Pattern(pattern: String) {
  require(!pattern.isEmpty, "pattern must not be empty")
  require(
    {
      val braces = pattern.filter { c => c == '[' || c == ']' }
      braces.isEmpty || braces == "[]"
    },
    "must contain single set of matched braces or no braces at all"
  )

  val (prefix, chars, suffix) = {
    val splitPrefix = pattern.split('[')
    val prefixOpt = splitPrefix.dropRight(1).headOption
    val remainder = splitPrefix.last

    val remainderSuffix = remainder.split(']')
    val suffixOpt = remainderSuffix.drop(1).headOption
    val allChars = prefixOpt.getOrElse("") + remainderSuffix.head + suffixOpt.getOrElse("")
    (prefixOpt, allChars, suffixOpt)
  }

  val cmap =
    chars.foldLeft(Map.empty[Char, Int]) { (map, c) =>
      val n = map.getOrElse(c, 0) + 1
      map + (c -> n)
    }

  // word must be lower case
  def matches(word: String): Boolean = {
    if (word.isEmpty) return false

    if (!prefix.forall { word.startsWith(_) }) {
      return false
    }

    if (!suffix.forall { word.endsWith(_) }) {
      return false
    }

    val result =
      word.foldLeft(cmap) { (map, c) =>
        val n = map.getOrElse(c, 0) - 1
        map + (c -> n)
      }

    result.values forall { _ >= 0 }
  }
}

object Dictionary {
  val ResourcePrefix = "resource:"

  private[this] val currentRef = new AtomicReference[Dictionary]

  def current = currentRef.get
  def current_=(dictionary: Dictionary): Dictionary = {
    currentRef.set(dictionary)
    dictionary
  }

  lazy val default = fromFile("resource:default_dictionary.txt")

  def inputStreamFromPath(path: String): Option[InputStream] = {
    if (path.startsWith(ResourcePrefix)) {
      val resourcePath = path.substring(ResourcePrefix.length)
      Option(getClass.getClassLoader.getResourceAsStream(resourcePath))
    } else {
      Some(new FileInputStream(path))
    }
  }

  def fromFile(path: String): Dictionary = {
    val dictionaryInputStream = inputStreamFromPath(path)
    if (dictionaryInputStream.isEmpty) {
      throw new Exception("resource '%s' not found".format(path))
    }

    val bloomFilterInputStream = inputStreamFromPath(path + ".bloom")

    new FastFragmentDictionary(new LineIterator(dictionaryInputStream.get), bloomFilterInputStream)
  }

  def fromInputStream(inputStream: InputStream): Dictionary = {
    new FastFragmentDictionary(new LineIterator(inputStream))
  }
}

trait Dictionary {
  def all: Seq[String]

  def contains(word: String): Boolean

  def matching(pattern: String): Dictionary

  def mightExpandToWord(fragment: String): Boolean
}

class TrivialDictionary(wordsIter: Iterator[String]) extends Dictionary {
  protected[this] val words = SortedSet.empty[String] ++ wordsIter

  lazy val all = words.toSeq

  def contains(word: String) = words.contains(word.toLowerCase)

  def matching(pattern: String): Dictionary = {
    val p = Pattern(pattern)

    new TrivialDictionary(words.iterator.filter { p.matches(_) })
  }

  def mightExpandToWord(fragment: String): Boolean = {
    val lowerFragment = fragment.toLowerCase
    if (lowerFragment.length == 1 && Character.isLetter(lowerFragment(0))) {
      return true
    }

    words.contains(lowerFragment) || words.exists { _.contains(lowerFragment) }
  }
}

class FastFragmentDictionary(
    wordsIter: Iterator[String],
    bloomFilterInputStream: Option[InputStream] = None)
  extends TrivialDictionary(wordsIter)
{
  private[this] val bloomFilter = {
    bloomFilterInputStream match {
      case Some(inputStream) => readBloomFilter(inputStream)
      case None              => constructBloomFilter(words)
    }
  }

  private[this] def readBloomFilter(inputStream: InputStream): BloomFilter[CharSequence] = {
    try {
      BloomFilter.readFrom(inputStream, Funnels.stringFunnel(Charsets.UTF_8))
    } finally {
      inputStream.close()
    }
  }

  private[this] def constructBloomFilter(words: Set[String]): BloomFilter[CharSequence] = {
    def applyFragments(f: (String) => Unit) {
      words.foreach { word =>
        val len = word.length
        if (len > 2) {
          (2 until len) foreach { window =>
            word.sliding(window).foreach { f(_) }
          }
        }
      }
    }

    var filterSize = 0
    applyFragments { _ => filterSize += 1 }

    val filter = BloomFilter.create(Funnels.stringFunnel(Charsets.UTF_8), filterSize, 0.01)
    applyFragments { s => filter.put(s) }
    filter
  }

  def writeBloomFilterTo(path: String) {
    val out = new BufferedOutputStream(new FileOutputStream(path))
    try {
      bloomFilter.writeTo(out)
    } finally {
      out.close()
    }
  }

  override def mightExpandToWord(fragment: String): Boolean = {
    val lowerFragment = fragment.toLowerCase
    if (lowerFragment.length == 1 && Character.isLetter(lowerFragment(0))) {
      return true
    }
    if (contains(lowerFragment)) return true

    bloomFilter.mightContain(lowerFragment)
  }
}
