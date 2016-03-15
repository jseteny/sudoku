import scala.collection.mutable

object Sudoku extends App {

  val x = null
  val t = List(
    //    List(x, x, 5, 6),
    //  List(x, 5, 7, 8),
    // List(1, 4, 9, 7)
    List(9, 8, x, x, 4, 5, 6, 7, 1),
    List(x, 9, 8, 1, 3, 6, 5, 2, 7),
    List(3, 7, 6, 8, x, 2, 4, 5, x)
  ).map(_.map(e => if (e == null) None else Option(e.asInstanceOf[Int])))

  val table = Table(t)

  val solution = SolveWithBackTrack(table, soFarSoGood)

  print(s"solution:\n${solution.mkString("\n")}")

  def soFarSoGood(table: Table, solution: List[Int]): Boolean = {

    if (rowAndColumnOk(merge(table, solution))) {
      println(solution.mkString("\n"))
      println()

      true

    } else {

      false
    }
  }

  def merge(table: Table, solution: List[Int]): Table = {
    var solutionList = solution

    val cells = table.cells.map(row => row.map { element => (element, solutionList) match {
      case (Some(n), _) =>
        Some(n)

      case (_, Nil) =>
        None // no solution yet for the given cell

      case (_, head :: tail) =>
        solutionList = tail
        Some(head)
    }
    })

    Table(cells)
  }

  def rowAndColumnOk(table: Table): Boolean = {
    val emptySet: Set[Int] = Set.empty
    val coluns = mutable.MutableList(table.cells.head.map(e => emptySet): _*)
    for (r <- table.cells.indices) {
      var row: Set[Int] = Set.empty
      for (c <- table.cells.head.indices) {
        val element = table.cells(r)(c)
        element match {

          case Some(n: Int) =>
            if (row.contains(n) || coluns(c).contains(n)) {
              return false
            } else {
              row += n
              coluns(c) += n
            }

          case None =>
          // no solution yet for the given cell
        }
      }
    }

    //noinspection RemoveRedundantReturn
    return true // all the cells has good solution
  }
}
