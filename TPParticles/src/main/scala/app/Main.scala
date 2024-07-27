package app

import scalafx.animation.Timeline.Indefinite
import scalafx.animation.{KeyFrame, Timeline}
import scalafx.beans.property.ObjectProperty
import scalafx.application.JFXApp3
import scalafx.application.JFXApp3.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.paint.Color.White
import scalafx.util.Duration

object Main extends JFXApp3 {

  private val windowSize = 1000
  val particleRadius = 5
  private val numParticles = 50
  private val particles: List[Particle] = Particle.createParticles(numParticles, windowSize, particleRadius)
  private val state: ObjectProperty[List[Particle]] = ObjectProperty(particles)

  override def start(): Unit = {
    stage = new PrimaryStage {
      title = "Particles"
      width = windowSize
      height = windowSize
      scene = new Scene() {
        fill = White
        content = state.value.map(_.draw)
        state.onChange {
          content = state.value.map(_.draw)
        }
      }
    }

    new Timeline {
      keyFrames = List(KeyFrame(time = Duration(50), onFinished = _ => updateState()))
      cycleCount = Indefinite
    }.play()
  }

  private def updateState(): Unit = {
    val updatedList = state.value.map(p => p.updatePosition(windowSize))
    state.update(updatedList.map(p => p.updateDirectionIfColliding(updatedList)))
  }
}
