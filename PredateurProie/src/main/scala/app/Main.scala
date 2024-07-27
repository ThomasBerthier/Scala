package app

import scalafx.application.JFXApp3
import scalafx.beans.property.ObjectProperty
import scalafx.animation.{KeyFrame, Timeline}
import scalafx.animation.Timeline.Indefinite
import scalafx.application.JFXApp3.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.paint.Color.White
import scalafx.util.Duration
import scalafx.scene.input.KeyCode

import scala.annotation.tailrec
import scala.util.Random
import EntityType.*
import app.Input.{DOWN, DOWN_LEFT, DOWN_RIGHT, LEFT, NONE, RIGHT, UP, UP_LEFT, UP_RIGHT}

import scalafx.Includes.jfxKeyCode2sfx

object Main extends JFXApp3 {

  private val cellSize = 20
  private val windowSize = 600
  private val cellNumber = windowSize / cellSize
  private val nPredatorsDefault = 1
  private val gameSpeedDefault = 1000 // In ms

  private val startingPositionState: ObjectProperty[Map[(Int, Int), EntityType]] = ObjectProperty(Map.empty)

  private val pressedKeys: ObjectProperty[Set[KeyCode]] = ObjectProperty(Set.empty[KeyCode])

  private val nPredatorsState: ObjectProperty[Int] = ObjectProperty(nPredatorsDefault)
  private val gameSpeedState: ObjectProperty[Int] = ObjectProperty(gameSpeedDefault)


  override def start(): Unit = {
    val args = parameters.raw
    nPredatorsState.update(if (args.nonEmpty) args.head.toIntOption.getOrElse(nPredatorsDefault) else nPredatorsDefault)
    gameSpeedState.update(if (args.length > 1) args(1).toIntOption.getOrElse(gameSpeedDefault) else gameSpeedDefault)
    val entities: List[Entity] = populate()
    val gridState: ObjectProperty[Grid] = ObjectProperty(Grid(cellSize, cellNumber, cellNumber, nPredatorsDefault, entities, startingPositionState.value))
    new Timeline {
      cycleCount = Indefinite
      keyFrames = Seq(
        KeyFrame(Duration(gameSpeedState.value), onFinished = _ => gridState.update(gridState.value.update(determineInput())))
      )
    }.play()

    stage = new PrimaryStage {
      title = "Proie-Predateurs"
      width = windowSize
      height = windowSize
      scene = new Scene() {
        fill = White
        content = gridState.value.drawGrid()
        gridState.onChange {
          content = gridState.value.drawGrid()
        }
        onKeyPressed = keyEvent => {
          pressedKeys.update(pressedKeys.value + keyEvent.getCode)
        }

        onKeyReleased = keyEvent => {
          pressedKeys.update(pressedKeys.value - keyEvent.getCode)
        }
      }
    }
  }

  private def determineInput(): Input = {
    val keys = pressedKeys.value
    if (keys.contains(KeyCode.Up) && keys.contains(KeyCode.Left)) UP_LEFT
    else if (keys.contains(KeyCode.Up) && keys.contains(KeyCode.Right)) UP_RIGHT
    else if (keys.contains(KeyCode.Down) && keys.contains(KeyCode.Left)) DOWN_LEFT
    else if (keys.contains(KeyCode.Down) && keys.contains(KeyCode.Right)) DOWN_RIGHT
    else if (keys.contains(KeyCode.Up)) UP
    else if (keys.contains(KeyCode.Down)) DOWN
    else if (keys.contains(KeyCode.Left)) LEFT
    else if (keys.contains(KeyCode.Right)) RIGHT
    else NONE
  }

  @tailrec
  private def randomPosition(grid: Map[(Int, Int), EntityType]): (Int, Int) = {
    val x = Random.nextInt(cellNumber)
    val y = Random.nextInt(cellNumber)
    if (grid.contains(x, y) && grid(x, y).eq(PREY) || grid.contains(x, y) && grid(x, y).eq(PREDATOR))
      randomPosition(grid) // Position already occupied, try again
    else
      (x, y)
  }

  private def populate(): List[Entity] = {
    val predators: List[Entity] = (1 to nPredatorsState.value).map { _ =>
      val (x, y) = randomPosition(startingPositionState.value)
      startingPositionState.update(startingPositionState.value.updated((x, y), PREDATOR))
      Predator(x, y)
    }.toList

    val (x, y) = randomPosition(startingPositionState.value)
    startingPositionState.update(startingPositionState.value.updated((x, y), PREY))
    val prey: Prey = Prey(x, y)

    predators :+ prey
  }
}



