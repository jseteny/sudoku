package algorithms.strings

import java.io.PrintWriter

import scala.collection.mutable

object Solution {

  // Complete the palindromeIndex function below.
  def palindromeIndex(s: String): Int = {
    if (isPalindrome(s)) -1
    else (0 until s.length).find(i => isPalindrome(s, Some(i))).getOrElse(-1)
  }

  def isPalindrome(s: String, index: Option[Int] = None): Boolean = {
    val len = if (index.isDefined) s.length - 1
    else s.length

    if (index.isEmpty)
      (0 until len / 2).forall { i => s(i) == s(len - 1 - i) }

    else

      (0 until len / 2).forall { i =>
        if (i < index.getOrElse()) s(i) == s(len - 1 - i)
      }
  }

  def main(args: Array[String]) {
    println(
      3, palindromeIndex("aaab")
    )
    println(
      0, palindromeIndex("baa")
    )
    println(
      -1, palindromeIndex("aaa")
    )
    /*
        val stdin = scala.io.StdIn
        val printWriter = new PrintWriter(sys.env("OUTPUT_PATH"))
        val q = stdin.readLine.trim.toInt
        for (qItr <- 1 to q) {
          val s = stdin.readLine
          val result = palindromeIndex(s)
          printWriter.println(result)
        }
        printWriter.close()
    */
  }
}
