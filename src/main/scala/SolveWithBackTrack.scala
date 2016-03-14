import java.util.NoSuchElementException

import scala.annotation.tailrec


object SolveWithBackTrack {

  def apply(table: Table, soFarSoGoodPredicate: (Table, List[Int]) => Boolean): List[Int] = {

    @tailrec def solve(i: Int, solution: List[Int]): List[Int] = {
      if (solution.size == table.emptyCellCount) {
        solution.reverse
      } else if (i <= 9 && soFarSoGoodPredicate(table, (i :: solution).reverse)) {
        solve(1, i :: solution)
      }
      else if (i <= 8) {
        solve(i + 1, solution)
      } else {
        solve(solution.head + 1, solution.tail)
      }
    }

    try {
      solve(1, Nil)
    } catch {
      case e: NoSuchElementException => Nil // No solution found
    }
  }
}
