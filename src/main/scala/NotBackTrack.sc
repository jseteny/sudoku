val ps = List(1, 2, 3).permutations.toList
ps.mkString("\n")
def soFarSoGood(table: List[List[Some[Int]]], solution: List[List[Int]]): Boolean = solution match {
  case List(2, 1, 3) :: List(3, 2, 1) :: List(2, 3, 1) :: Nil => true
  case List(3, 2, 1) :: List(2, 3, 1) :: Nil => true
  case List(2, 3, 1) :: Nil => true
  case _ => false
}

class Solve(table: List[List[Some[Int]]], permutations: List[List[Int]], soFarSoGoodPredicate: (List[List[Some[Int]]], List[List[Int]]) => Boolean) {
  def solve(i: Int, s: List[List[Int]]): List[List[Int]] = {
    if (s.size == 3) {
      s
    } else if (soFarSoGoodPredicate(table, permutations(i) :: s)) {
      solve(0, permutations(i) :: s)
    }
    else if (i < permutations.size - 1) {
      solve(i + 1, s)
    } else {
      s.tail
    }
  }
}

object Solve {
  def apply(table: List[List[Some[Int]]], permutations: List[List[Int]], soFarSoGoodPredicate: (List[List[Some[Int]]], List[List[Int]]) => Boolean): List[List[Int]] = {
    new Solve(table, permutations, soFarSoGoodPredicate).solve(0, Nil)
  }
}

val solution = Solve(Nil, ps, soFarSoGood)



val ps1 = List(2, 3, 1)
solve1(0, Nil)
def solve1(i: Int, s: List[Int]): List[Int] = {
  if (s.size == 3) {
    s
  } else if (soFarSoGood1(ps1(i) :: s)) {
    solve1(0, ps1(i) :: s)
  }
  else if (i < ps1.size - 1) {
    solve1(i + 1, s)
  } else {
    s.tail
  }
}

def soFarSoGood1(s: List[Int]): Boolean = s match {
  case 2 :: 1 :: 3 :: Nil => true
  case 1 :: 3 :: Nil => true
  case 3 :: Nil => true
  case _ => false
}