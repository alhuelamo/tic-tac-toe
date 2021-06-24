type Pos = (Int, Int)

val ls: List[Pos] = List(
  (0, 0),
  (0, 1),
)

println(
  ls
    .combinations(3)
    .toList
    .exists(_ == (0,0))
)