import java.util.NoSuchElementException

import scala.annotation.tailrec


object SolveWithBackTrack {

  def apply[Table](table: Table,
                   soFarSoGood: (Table, List[Int]) => Boolean,
                   finished: (Table, List[Int]) => Boolean,
                   first: Int,
                   max: Int
                  ): List[Int] = {

    @tailrec def solve(i: Int, solution: List[Int]): List[Int] = {
      if (finished(table, solution)) {
        solution.reverse
      } else if (i <= max && soFarSoGood(table, (i :: solution).reverse)) {
        solve(first, i :: solution)
      }
      else if (i <= max - 1) {
        solve(i + 1, solution)
      } else {
        solve(solution.head + 1, solution.tail)
      }
    }

    try {
      solve(first, Nil)
    } catch {
      case e: NoSuchElementException => Nil // No solution found
    }
  }
}
