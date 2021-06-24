@main def simulate: Unit = {
  val finalStats = (0 until 1000000)
    .foldLeft(Stats.empty) { (accStats, _) =>
      Game.play(Game.init(), accStats)
    }
  println(finalStats)
}
