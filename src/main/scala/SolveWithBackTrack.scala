import scala.annotation.tailrec

object SolveWithBackTrack {
  def apply(table: List[List[Option[Int]]], permutations: List[List[Int]], soFarSoGoodPredicate: (List[List[Option[Int]]], List[List[Int]]) => Boolean): List[List[Int]] = {

    @tailrec def solve(i: Int, solution: List[List[Int]]): List[List[Int]] = {
      if (solution.size == table.size) {
        solution
      } else if (soFarSoGoodPredicate(table, permutations(i) :: solution)) {
        solve(0, permutations(i) :: solution)
      }
      else if (i < permutations.size - 1) {
        solve(i + 1, solution)
      } else {
        solution.tail
      }
    }

    solve(0, Nil)
  }
}