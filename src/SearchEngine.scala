
object SearchEngine {
  def main(args: Array[String]) {
    val ret = searchSubString("abcbccab", "b")
    println(ret)
  }

  def searchSubString(str: String, substr: String): Array[Map[String,String]] = {
    val suffix_array = createSuffixArray(str)
    val p = binarySearch(suffix_array, substr)
    if (p == -1) {
      Array[Map[String,String]]()
    } else {
      val start = searchIterative(suffix_array, substr, p, 0, -1)
      val end = searchIterative(suffix_array, substr, p, suffix_array.length, 1)
      suffix_array.slice(start, end+1)
    }
  }

  def createSuffixArray(str: String): Array[Map[String,String]] = {
    (str.length to 1 by -1)
      .map {(i) => Map("s"+(str.length-i+1).toString -> str.takeRight(i))}
      .sortBy(_.values.head)
      .toArray
  }

  def binarySearch(suffix_array: Array[Map[String,String]], substr: String): Int = {
    if (suffix_array.length == 0) return -1

    val p = suffix_array.size/2
    val str = suffix_array(p).values.head

    if (str.startsWith(substr)) {
      p
    } else if (str < substr) {
      binarySearch(suffix_array.slice(p+1, suffix_array.length), substr)
    } else {
      binarySearch(suffix_array.slice(0, p), substr)
    }
  }

  def searchIterative(suffix_array: Array[Map[String,String]], substr: String, p: Int, end: Int, direction: Int): Int = {
    for (i <- p to end by direction) {
      if (!suffix_array(i).values.head.startsWith(substr)) {
        return i - direction
      }
    }
    p
  }
}
