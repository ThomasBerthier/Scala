package app

sealed trait Entity(val x:Int, val y:Int)

final case class Predator(override val x: Int, override val y: Int) extends Entity(x, y)

final case class Prey(override val x: Int, override val y: Int) extends Entity(x, y)
