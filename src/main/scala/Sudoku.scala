

object Sudoku extends App {

  val x = null
  val table = List(
    List(1, x, x, 4, 5, 6, 7, 8, 9),
    List(x, 2, 3, x, x, x, x, x, x)
  ).map(_.map(e=>if (e==null) None else Option(e.asInstanceOf[Int])))

  val ps = List(1, 2, 3, 4, 5, 6, 7, 8, 9).permutations.toList
  println(ps.size)

  val solution = SolveWithBackTrack(table, ps, soFarSoGood)

  print(solution.mkString("\n"))

  def soFarSoGood(table: List[List[Option[Int]]], solution: List[List[Int]]): Boolean = {
    val fit: Boolean = fitTheTable(table, solution)
    if(fit) {
      println(solution.mkString("\n"))
      println()
    }

    fit && columnsOk(solution)
  }

  def columnsOk(solution: List[List[Int]]): Boolean = {
    for (column <- 0 to 8) {
      var s: Set[Int] = Set.empty
      for (row <- solution.indices) {
        val e = solution(row)(column)
        if (s.contains(e))
          return false
        else
          s += e
      }
    }
    true
  }

  def fitTheTable(table: List[List[Option[Int]]],solution: List[List[Int]]): Boolean = {
    solution.zipWithIndex.forall{case (row,i) => row.corresponds(table(i))((pe, te) => te.isEmpty || te.get == pe)}
  }
}
