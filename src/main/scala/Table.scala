
case class Table( cells: List[List[Option[Int]]]) {

  def emptyCellCount:Int  = cells.map(_.count(_.isEmpty)).sum
}
