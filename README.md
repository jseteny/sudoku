```scala
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
```
