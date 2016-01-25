import java.util.NoSuchElementException

import scala.annotation.tailrec


object SolveWithBackTrack {

  def apply(table: List[List[Option[Int]]], permutations: List[List[Int]], soFarSoGoodPredicate: (List[List[Option[Int]]], List[List[Int]]) => Boolean): List[List[Int]] = {

    /**
      * Collects the index of permutations according to soFarSoGoodPredicate, but in reverse order.
      *
      * @param i next index to try
      * @param solution the solution so far
      * @return it will return only when we collected the needed size of solution always coming through soFarSoGoodPredicate == true
      */
    @tailrec def solve(i: Int, solution: List[Int]): List[Int] = {
      if (solution.size == table.size) {
        solution
      } else if (i < permutations.size && soFarSoGoodPredicate(table, m(i :: solution))) {
        solve(0, i :: solution)
      }
      else if (i < permutations.size - 1) {
        solve(i + 1, solution)
      } else {
        solve(solution.head + 1, solution.tail)
      }
    }

    /**
      * Map from list of indices to actual solution list.
      */
    def m(s: List[Int]): List[List[Int]] = s.map(index => permutations(index)).reverse

    try {
      m(solve(0, Nil))
    } catch {
      case e: NoSuchElementException => Nil // No solution found
    }
  }
}
