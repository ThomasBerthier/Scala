package app

import scalafx.application.JFXApp3
import scalafx.beans.property.ObjectProperty
import FishType.*
import scalafx.animation.{KeyFrame, Timeline}
import scalafx.animation.Timeline.Indefinite
import scalafx.application.JFXApp3.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.paint.Color.White
import scalafx.util.Duration

import scala.annotation.tailrec
import scala.util.Random

object Main extends JFXApp3 {

  private val cellSize = 20
  private val windowSize = 600
  private val cellNumber = windowSize/cellSize
  private val nTunas = 30
  private val nSharks = 10
  private val defaultTunaBreedingCycle = 2
  private val defaultSharkBreedingCycle = 8
  private val defaultSharkEnergy = 25
  private val startingPositionState: ObjectProperty[Map[(Int, Int), FishType]] = ObjectProperty(Map.empty)
  private val fishes: List[Fish] = populate()
  private val gridState: ObjectProperty[Grid] = ObjectProperty(Grid(cellSize, cellNumber, cellNumber, nTunas, nSharks, defaultSharkEnergy, defaultSharkBreedingCycle, defaultTunaBreedingCycle, fishes, startingPositionState.value))

  override def start(): Unit = {

    new Timeline {
      cycleCount = Indefinite
      keyFrames = Seq(
        KeyFrame(Duration(200), onFinished = _ => gridState.update(gridState.value.update()))
      )
    }.play()

    stage = new PrimaryStage {
      title = "wa-Tor"
      width = windowSize
      height = windowSize
      scene = new Scene() {
        fill = White
        content = gridState.value.drawGrid()
        gridState.onChange {
          content = gridState.value.drawGrid()
        }
      }
    }
  }

  @tailrec
  private def randomPosition(grid: Map[(Int, Int), FishType]): (Int, Int) = {
    val x = Random.nextInt(cellNumber)
    val y = Random.nextInt(cellNumber)
    if (grid.contains(x, y) && grid(x, y).eq(SHARK) || grid.contains(x, y) && grid(x, y).eq(TUNA))
      randomPosition(grid) // Position already occupied, try again
    else
      (x, y)
  }

  private def populate(): List[Fish] = {
    val sharks: List[Fish] = (1 to nSharks).map { _ =>
      val (x, y) = randomPosition(startingPositionState.value)
      startingPositionState.update(startingPositionState.value.updated((x, y), SHARK))
      Shark(x, y, 8, 10)
    }.toList

    val tunas: List[Fish] = (1 to nTunas).map(_ => {
      val (x, y) = randomPosition(startingPositionState.value)
      startingPositionState.update(startingPositionState.value.updated((x,y),TUNA))
      Tuna(x, y, 5)
    }).toList

    sharks ++ tunas
  }
}



