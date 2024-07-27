package app

import app.Directions.{DOWN, DOWNLEFT, DOWNRIGHT, LEFT, RIGHT, UP, UPLEFT, UPRIGHT}
import scalafx.scene.paint.Color
import scalafx.scene.shape.Circle

import scala.util.Random

final case class Particle(x:Double, y:Double, color:Color, direction: Directions.Value, particleRadius: Int) {
  def draw: Circle = new Circle {
    centerX = x
    centerY = y
    radius = particleRadius
    fill = color

  }

  private def isColliding(other: Particle): Boolean = {
    val dx = x - other.x
    val dy = y - other.y
    val distance = math.sqrt(dx * dx + dy * dy)
    distance < 2 * particleRadius
  }

  def updatePosition(window: Double): Particle = {
    val (newX, newY) = direction match {
      case UP => (x, y - particleRadius)
      case DOWN => (x, y + particleRadius)
      case LEFT => (x - particleRadius, y)
      case RIGHT => (x + particleRadius, y)
      case UPRIGHT => (x + particleRadius, y - particleRadius)
      case UPLEFT => (x - particleRadius, y - particleRadius)
      case DOWNRIGHT => (x + particleRadius, y + particleRadius)
      case DOWNLEFT => (x - particleRadius, y + particleRadius)
    }

    val wrappedX = if (newX < 0) window else if (newX > window) 0 else newX
    val wrappedY = if (newY < 0) window else if (newY > window) 0 else newY

    copy(x = wrappedX, y = wrappedY)
  }

  def updateDirectionIfColliding(others: List[Particle]): Particle = {
    val colliding = others.exists(other => this != other && isColliding(other))
    val newDirection = if (colliding) Directions.randomDirection() else direction
    copy(direction = newDirection)
  }
}

object Particle {
  def createRandom(windowHeight: Int, windowWidth: Int, particleRadius: Int): Particle = {
    val x = Random.nextDouble() * windowWidth
    val y = Random.nextDouble() * windowHeight
    val color = Color.color(Random.nextDouble(), Random.nextDouble(), Random.nextDouble())
    val direction = Directions.randomDirection()
    Particle(x, y, color, direction, particleRadius)
  }

  def createParticles(numParticles: Int, windowSize: Int, particleRadius: Int): List[Particle] = {
    List.fill(numParticles)(Particle.createRandom(windowSize, windowSize, particleRadius))
  }
}
