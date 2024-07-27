package app

object Directions extends Enumeration {
  type Directions = Value
  val UP, DOWN, RIGHT, LEFT, UPRIGHT, UPLEFT, DOWNLEFT, DOWNRIGHT = Value

  def randomDirection(): Directions.Value = {
    val directions = Directions.values.toSeq
    directions(scala.util.Random.nextInt(directions.size))
  }
}
