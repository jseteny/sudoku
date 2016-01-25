import scala.annotation.tailrec

object ReallyBackTrack extends App {
  val ps1 = List(1, 2, 3)

  m(solve1(0, Nil))


  @tailrec def solve1(i: Int, solution: List[Int]): List[Int] = {
    if (solution.size == 3) {
      solution
    } else if (i < ps1.size && soFarSoGood1(m(i :: solution))) {
      solve1(0, i :: solution)
    }
    else if (i < ps1.size - 1) {
      solve1(i + 1, solution)
    } else {
      solve1(solution.head + 1, solution.tail)
    }
  }

  def m(s: List[Int]) = s.map(ps1(_)).reverse

  def soFarSoGood1(s: List[Int]): Boolean = {

    println(s)

    s match {
      case 3 :: 1 :: 2 :: Nil => true
      case 3 :: 1 :: Nil => true
      case 3 :: Nil => true
      case 2 :: 3 :: Nil => true
      case 2 :: 2 :: Nil => true
      case 2 :: Nil => true
      case 1 :: Nil => true
      case _ => false
    }
  }
}