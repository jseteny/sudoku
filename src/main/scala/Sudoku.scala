

object Sudoku extends App {

  val x = null
  val table = List(
    List(8, x, x, 4, 5, 6, 7, 1),
    List(x, 8, 1, 3, 6, 5, 2, 7),
    List(7, 6, 8, x, 2, 4, 5, 3)
  ).map(_.map(e => if (e == null) None else Option(e.asInstanceOf[Int])))

  val ps = List(1, 2, 3, 4, 5, 6, 7, 8).permutations.toList
  println(ps.size)

  val solution = SolveWithBackTrack(table, ps, soFarSoGood)

  print(solution.mkString("\n"))

  def soFarSoGood(table: List[List[Option[Int]]], solution: List[List[Int]]): Boolean = {

    val fit: Boolean = fitTheTable(table, solution)
    if (fit && columnsOk(solution)) {
      println(solution.mkString("\n"))
      println(s"fit: $fit, ok: ${true}")
      println()

      true

    } else {

      false
    }
  }

  def columnsOk(solution: List[List[Int]]): Boolean = {
    for (column <- table.head.indices) {
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

  def fitTheTable(table: List[List[Option[Int]]], solution: List[List[Int]]): Boolean = {
    solution.par.zipWithIndex.forall { case (row, i) => row.corresponds(table(i))((se, te) => te.isEmpty || te.get == se) }
  }
}
