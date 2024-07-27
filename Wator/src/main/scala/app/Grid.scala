package app

import FishType.*
import scalafx.scene.paint.Color.{Blue, Green, Red}
import scalafx.scene.shape.Rectangle

import scala.util.Random

case class Grid(cellSize: Int, widthCellNumber: Int, heightCellNumber: Int, nTunas: Int, nSharks: Int, defaultSharkEnergy: Int, defaultSharkBreedingCycle: Int, defaultTunaBreedingCycle: Int, fishes: List[Fish], grid: Map[(Int, Int), FishType]){

  private def findEmptyAdjacentCells(x: Int, y: Int, currentGrid: Map[(Int, Int), FishType]): List[(Int, Int)] = {
    val positions = List((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1),
                         (x + 1, y + 1), (x + 1, y - 1), (x - 1, y + 1), (x - 1, y - 1))
    positions.filter { case (i, j) =>
      i >= 0 && i < widthCellNumber && j >= 0 && j < heightCellNumber && !currentGrid.contains((i,j))
    }
  }
  
  private def findTunaAdjacentCells(x: Int, y: Int, currentGrid: Map[(Int, Int), FishType]): List[(Int, Int)] = {
    val positions = List((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1),
                         (x + 1, y + 1), (x + 1, y - 1), (x - 1, y + 1), (x - 1, y - 1))
    positions.filter { case (i, j) =>
      i >= 0 && i < widthCellNumber && j >= 0 && j < heightCellNumber && currentGrid.contains((i,j)) && currentGrid((i, j)) == TUNA
    }
  }

  def drawGrid(): List[Rectangle] = {
    grid.map(element => {
      new Rectangle {
        x = element._1._1 * cellSize
        y = element._1._2 * cellSize
        width = cellSize
        height = cellSize
        fill =
          element._2 match {
            case TUNA => Green
            case SHARK => Red
          }
      }
    }).toList
  }

  def update(): Grid = {
    val newState = fishes.foldLeft((grid, List.empty[Fish], Set.empty[(Int,Int)])) { (state, fish) =>
      val (currentGrid, updatedFishes, processed) = state
      fish match {
        case Tuna(x, y, breedingCycle) if !processed.contains((x,y)) =>
          val emptyCells = findEmptyAdjacentCells(x, y, currentGrid)
          if (emptyCells.nonEmpty) {
            val (newX, newY) = emptyCells(Random.nextInt(emptyCells.length))
            val newGrid = currentGrid.updated((newX, newY), TUNA)
            val newBreedingCycle = breedingCycle - 1
            if (newBreedingCycle <= 0) {
              (newGrid, updatedFishes :+ Tuna(newX, newY, defaultTunaBreedingCycle) :+ Tuna(x, y, defaultTunaBreedingCycle), processed + ((newX,newY)))
            } else {
              val newGridWithDeleted = newGrid.removed((x, y))
              (newGridWithDeleted, updatedFishes :+ Tuna(newX, newY, newBreedingCycle), processed + ((newX,newY)))
            }
          } else {
            (currentGrid, updatedFishes :+ Tuna(x, y, breedingCycle - 1), processed + ((x,y)))
          }
        case Shark(x, y, breedingCycle, energy) if !processed.contains((x,y))=>
          val tunaCells = findTunaAdjacentCells(x, y, currentGrid)
          if (tunaCells.nonEmpty) {
            val (tunaX, tunaY) = tunaCells(Random.nextInt(tunaCells.length))
            val newGrid = currentGrid.removed((tunaX, tunaY)).updated((tunaX, tunaY), SHARK)
            val newEnergy = energy + 2
            val newBreedingCycle = breedingCycle - 1
            if (newBreedingCycle <= 0) {
              (newGrid, Shark(tunaX, tunaY, defaultSharkBreedingCycle, newEnergy) :: Shark(x, y, defaultSharkBreedingCycle, defaultSharkEnergy) :: updatedFishes, processed + ((tunaX,tunaY)))
            } else {
              val newGridWithDeleted = newGrid.removed((x, y))
              (newGridWithDeleted, Shark(tunaX, tunaY, newBreedingCycle, newEnergy) :: updatedFishes, processed + ((tunaX,tunaY)))
            }
          } else {
            val emptyCells = findEmptyAdjacentCells(x, y, currentGrid)
            if (emptyCells.nonEmpty) {
              val (newX, newY) = emptyCells(Random.nextInt(emptyCells.length))
              val newGrid = currentGrid.updated((newX, newY), SHARK)
              val newEnergy = energy - 1
              val newBreedingCycle = breedingCycle - 1
              if (newEnergy <= 0) {
                val newGridWithDeleted = newGrid.removed((x, y)).removed((newX, newY))
                (newGridWithDeleted, updatedFishes, processed)
              } else if (newBreedingCycle <= 0) {
                (newGrid, Shark(newX, newY, defaultSharkBreedingCycle, newEnergy) :: Shark(x, y, defaultSharkBreedingCycle, defaultSharkEnergy) :: updatedFishes, processed + ((newX,newY)))
              } else {
                val newGridWithDeleted = newGrid.removed((x, y))
                (newGridWithDeleted, Shark(newX, newY, newBreedingCycle, newEnergy) :: updatedFishes, processed + ((newX,newY)))
              }
            } else {
              (currentGrid, Shark(x, y, breedingCycle - 1, energy - 1) :: updatedFishes, processed + ((x,y)))
            }
          }
        case _ => state
      }
    }
    if(newState._1.values.forall(_ == TUNA) || newState._1.values.forall(_ == SHARK)) System.exit(0)
    Grid(cellSize, widthCellNumber, heightCellNumber, nTunas, nSharks, defaultSharkEnergy, defaultSharkBreedingCycle, defaultTunaBreedingCycle, newState._2, newState._1)
  }
  
}
