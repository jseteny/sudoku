package algorithms.strings

import java.io.PrintWriter

import scala.collection.mutable

object Solution {

  // Complete the palindromeIndex function below.
  def palindromeIndex(s: String): Int = {
    if (isPalindrome(s.toBuffer)) -1
    else (0 until s.length).find(i => pIndex(s, i)).getOrElse(-1)
  }

  def pIndex(s: String, index: Int): Boolean = {
    val newS = s.toBuffer
    newS.remove(index)
    isPalindrome(newS)
  }

  def isPalindrome(s: mutable.Buffer[Char]): Boolean = {
    val len = s.length
    (0 until len / 2).forall(i => s(i) == s(len - 1 - i))
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
