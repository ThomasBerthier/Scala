package app

sealed trait Fish(val x:Int, val y:Int, val breedingCycle: Int)

final case class Shark(override val x: Int, override val y: Int, override val breedingCycle: Int, energy: Int) extends Fish(x, y, breedingCycle)

final case class Tuna(override val x: Int, override val y: Int, override val breedingCycle: Int) extends Fish(x, y, breedingCycle)