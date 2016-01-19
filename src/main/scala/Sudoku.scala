
object Sudoku extends App {

  val x = null
  val table = List(
    List(x, x, x, x, x, x, x, x, x),
    List(x, x, x, x, x, x, x, x, x)
  )

  val ps = List(1, 2, 3, 4, 5, 6, 7, 8, 9).permutations

  val solution = solve(1, Nil)
  print(solution.mkString("\n"))

  def columnsOk(psf: List[List[Int]]): Boolean = {
    for (column <- 0 to 8) {
      var s: Set[Int] = Set.empty
      for (row <- 0 to 1) {
        val e = psf(row)(column)
        if (s.contains(e))
          return false
        else
          s += e
      }
    }
    true
  }

  def solve(nextRow: Int, psf: List[List[Int]]): List[List[Int]] = {
    if (nextRow >= 0) {
      for (p <- ps) {
        if (p.corresponds(table(nextRow))((pe, te) => te == null || te == pe) &&
          columnsOk(p :: psf)) {

          return solve(nextRow - 1, p :: psf)
        }
      }

      psf.tail
    } else {

      if (columnsOk(psf))
        psf
      else
        psf.tail
    }
  }
}
