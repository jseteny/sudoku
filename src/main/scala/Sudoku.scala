import scala.collection.mutable

object Sudoku extends App {

  val x = null

  val t = List(
    List(x, x, 5),
    List(x, 6, 7),
    List(1, 4, 9)
  ).map(_.map(e => if (e == null) None else Option(e.asInstanceOf[Int])))

  val websudoku_com_hard = List(
    //  (1, 2, 3, 4, 5, 6, 7, 8, 9)

    List(x, x, x, x, x, x, 8, x, x),
    List(9, x, x, 6, 8, x, x, 1, 4),
    List(x, 5, x, 7, x, x, 2, x, x),

    List(x, x, x, 8, x, x, 6, x, 2),
    List(7, x, x, 1, x, 4, x, x, 3),
    List(2, x, 4, x, x, 6, x, x, x),

    List(x, x, 9, x, x, 8, x, 5, x),
    List(8, 6, x, x, 4, 3, x, x, 7),
    List(x, x, 5, x, x, x, x, x, x)
  ).map(_.map(e => if (e == null) None else Option(e.asInstanceOf[Int])))


  // http://www.websudoku.com/?level=4&set_id=9612062736
  val websudoku_com_evil = List(
    //  (1, 2, 3, 4, 5, 6, 7, 8, 9)

    List(x, 9, x, x, x, 3, x, 7, 2),
    List(x, x, x, x, 9, 4, x, 6, x),
    List(x, x, x, 5, x, x, x, x, x),

    List(x, x, 4, x, x, x, x, 1, 6),
    List(7, x, 8, x, x, x, 5, x, 9),
    List(1, 2, x, x, x, x, 3, x, x),

    List(x, x, x, x, x, 7, x, x, x),
    List(x, 4, x, 9, 1, x, x, x, x),
    List(2, 6, x, 4, x, x, x, 9, x)
  ).map(_.map(e => if (e == null) None else Option(e.asInstanceOf[Int])))

  case class Table(cells: List[List[Option[Int]]]) {

    def emptyCellCount: Int = cells.map(_.count(_.isEmpty)).sum
  }

  val table = Table(websudoku_com_evil)

  val solution = SolveWithBackTrack(table, soFarSoGood, finished = (t: Table, solution) => t.emptyCellCount == solution.size, 1, 9)

  print(s"\nsolution:\n${merge(table, solution).cells.map(_.map(_.get)).mkString("\n")}\n")

  def soFarSoGood(table: Table, solution: List[Int]): Boolean = {

    val merged: Table = merge(table, solution)

    if (rowAndColumnOk(merged) &&
      threeByThreeCellOk(merged, 0 to 2, 0 to 2) &&
      threeByThreeCellOk(merged, 0 to 2, 3 to 5) &&
      threeByThreeCellOk(merged, 0 to 2, 6 to 8) &&

      threeByThreeCellOk(merged, 3 to 5, 0 to 2) &&
      threeByThreeCellOk(merged, 3 to 5, 3 to 5) &&
      threeByThreeCellOk(merged, 3 to 5, 6 to 8) &&

      threeByThreeCellOk(merged, 6 to 8, 0 to 2) &&
      threeByThreeCellOk(merged, 6 to 8, 3 to 5) &&
      threeByThreeCellOk(merged, 6 to 8, 6 to 8)) {

      print(".")
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


  def threeByThreeCellOk(table: Table, rows: Range, columns: Range): Boolean = {
    var cells: Set[Int] = Set.empty
    for (r <- rows) {
      for (c <- columns) {
        table.cells(r)(c) match {

          case Some(n: Int) =>
            if (cells.contains(n)) {
              return false
            } else {
              cells += n
            }

          case None =>
          // no solution yet for the given cell
        }
      }
    }

    //noinspection RemoveRedundantReturn
    return true // all the cells has good solution
  }


  def rowAndColumnOk(table: Table): Boolean = {
    val emptySet: Set[Int] = Set.empty
    val coluns = mutable.MutableList(table.cells.head.map(e => emptySet): _*)
    for (r <- table.cells.indices) {
      var row: Set[Int] = Set.empty
      for (c <- table.cells.head.indices) {
        table.cells(r)(c) match {

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
