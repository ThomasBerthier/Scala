package app

import app.EntityType.{PREDATOR, PREY}
import app.Input.{DOWN, DOWN_LEFT, DOWN_RIGHT, LEFT, NONE, RIGHT, UP, UP_LEFT, UP_RIGHT}
import scalafx.scene.paint.Color.{Blue, Green, Red}
import scalafx.scene.shape.Rectangle

import scala.util.Random

case class Grid(cellSize: Int, widthCellNumber: Int, heightCellNumber: Int, nPredators: Int, fishes: List[Entity], grid: Map[(Int, Int), EntityType]){

  private def findFastestPath(x: Int, y: Int, currentGrid: Map[(Int, Int), EntityType], occupied: Set[(Int, Int)]): (Int, Int) = {
    val ((preyX, preyY), entity): ((Int, Int), EntityType) = currentGrid.filter {
      case (_, entityType) => entityType == PREY
    }.head

    val potentialMoves = List(
      (x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1),
      (x - 1, y - 1), (x + 1, y - 1), (x - 1, y + 1), (x + 1, y + 1)
    )

    val validMoves = potentialMoves
      .filter { case (i, j) =>
        isInBorder(i, j) && !occupied.contains((i, j))
      }

    validMoves
      .minBy { case (i, j) =>
        math.abs(preyX - i) + math.abs(preyY - j)
      }
  }
  
  private def findPreyAdjacentCells(x: Int, y: Int, currentGrid: Map[(Int, Int), EntityType]): Option[(Int, Int)] = {
    val positions = List((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1),
                         (x + 1, y + 1), (x + 1, y - 1), (x - 1, y + 1), (x - 1, y - 1))

    positions
      .find { case (i, j) =>
        i >= 0 && i < widthCellNumber && j >= 0 && j < heightCellNumber &&
          currentGrid.contains((i, j)) && currentGrid((i, j)) == PREY
      }
  }

  private def isOccupied(x: Int, y: Int, currentGrid: Map[(Int, Int), EntityType]): Boolean = {
    currentGrid.contains((x, y))
  }

  private def isInBorder(x: Int, y: Int): Boolean = {
    x >= 0 && x < widthCellNumber && y >= 0 && y < heightCellNumber
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
            case PREY => Green
            case PREDATOR => Red
          }
      }
    }).toList
  }

  def update(userInput: Input): Grid = {
    val newState = fishes.foldLeft((grid, List.empty[Entity], Set.empty[(Int,Int)], false)) { (state, entity) =>
      val (currentGrid, updatedEntities, processed, hasDied) = state
      entity match {
        case Prey(x, y) if !processed.contains((x,y)) && !hasDied =>
          val (newX, newY) = userInput match {
            case UP    => (x, y - 1)
            case DOWN  => (x, y + 1)
            case LEFT  => (x - 1, y)
            case RIGHT => (x + 1, y)
            case UP_LEFT => (x - 1, y - 1)
            case UP_RIGHT => (x + 1, y - 1)
            case DOWN_LEFT => (x - 1, y + 1)
            case DOWN_RIGHT => (x + 1, y + 1)
            case NONE       => (x, y)
          }
          if(isOccupied(newX, newY, currentGrid) || !isInBorder(newX, newY)){
            val newGrid = currentGrid.removed((x, y)).updated((x, y), PREY)
            (newGrid, updatedEntities :+ Prey(x, y), processed + ((newX, newY)), false)
          }else{
            val newGrid = currentGrid.removed((x, y)).updated((newX, newY), PREY)
            (newGrid, updatedEntities :+ Prey(newX, newY), processed + ((newX, newY)), false)
          }
        case Predator(x, y) if !processed.contains((x,y)) && !hasDied =>
          val preyCell = findPreyAdjacentCells(x, y, currentGrid)
          preyCell match {
            case Some(newX,newY) =>
              val newGrid = currentGrid.removed((x, y)).updated((newX, newY), PREDATOR)
              (newGrid, Predator(newX, newY) :: updatedEntities, processed + ((newX,newY)), true)
            case None =>
              val occupied = currentGrid.keys.filter(currentGrid(_) == PREDATOR).toSet
              val (newX, newY) = findFastestPath(x, y, currentGrid, occupied)
              val newGrid = currentGrid.removed((x, y)).updated((newX, newY), PREDATOR)
              (newGrid, Predator(newX, newY) :: updatedEntities, processed + ((newX,newY)), false)
          }
        case _ => state
      }
    }
    if(newState._1.values.forall(_ == PREDATOR)) System.exit(0)
    Grid(cellSize, widthCellNumber, heightCellNumber, nPredators, newState._2, newState._1)
  }
  
}
