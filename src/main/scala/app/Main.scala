package app

import app.Simulation.{BallRadiusMeters, FrameDelayMillis, PixelsPerMeter, SimStepsPerFrame, iOrigin, jOrigin}
import org.scalajs.dom
import org.scalajs.dom.{document, html}
import org.scalajs.dom.html.Canvas

import scala.scalajs.js.timers.{SetTimeoutHandle, setTimeout}

object Main {

  private def ballMassEl: html.Input = document.getElementById("ballMassForm").asInstanceOf[html.Input]
  private def springRestLengthEl: html.Input = document.getElementById("springRestLengthForm").asInstanceOf[html.Input]
  private def springConstEl: html.Input = document.getElementById("springConstForm").asInstanceOf[html.Input]
  private def canvasEl: html.Canvas = document.getElementById("SimCanvas").asInstanceOf[Canvas]
  private def restartBtn: html.Button = document.getElementById("restartBtn").asInstanceOf[html.Button]

  def main(args: Array[String]): Unit = {

    ballMassEl.value = Simulation.BallMass.toString
    springRestLengthEl.value = Simulation.SpringRestLength.toString
    springConstEl.value = Simulation.SpringConst.toString

    document.addEventListener("DOMContentLoaded", { (e: dom.Event) =>
      start()
    })
  }

  private var simulation: Simulation = _

  private def start(): Unit = {
    val canvas = canvasEl
    canvas.addEventListener("mousedown", onMouseDown)
    canvas.addEventListener("mouseup", onMouseUp)
    canvas.addEventListener("mousemove", onMouseMove)
    canvas.addEventListener("mouseleave", onMouseLeave)

    setupSimulation()
    animationFrame()
    restartBtn.onclick = (e: dom.Event) => {
      setupSimulation()
    }
  }

  private def setupSimulation(): Unit = {
    val ballMass = ballMassEl.value.toDouble
    val springRestLength = springRestLengthEl.value.toDouble
    val springConst = springConstEl.value.toDouble

    simulation = initWorld(ballMass, springRestLength, springConst)
  }

  def animationFrame(): SetTimeoutHandle = {
    val dt = (0.001 * FrameDelayMillis) / SimStepsPerFrame
    (0 until SimStepsPerFrame).foreach { i =>
      simulation.update(dt)
    }
    render(simulation)
    setTimeout(FrameDelayMillis) { animationFrame() }
  }


  def onMouseDown(evt: dom.MouseEvent): Unit = {
    val canvas = canvasEl
    val hor = evt.pageX - canvas.offsetLeft
    val ver = evt.pageY - canvas.offsetTop
    val x = worldX(hor)
    val y = worldY(ver)
    simulation.grab(x.toInt, y.toInt)
  }

  def onMouseUp(evt: dom.MouseEvent): Unit = {
    simulation.release()
  }

  def onMouseMove(evt: dom.MouseEvent): Unit = {
    val canvas = canvasEl
    val hor = evt.pageX - canvas.offsetLeft
    val ver = evt.pageY - canvas.offsetTop
    val x = worldX(hor)
    val y = worldY(ver)
    simulation.pull(x, y)
  }

  def onMouseLeave(evt: dom.MouseEvent): Unit = {
    simulation.release();
  }


  private def initWorld(ballMass: Double, springRestLength: Double, springConst: Double): Simulation = {
    val x = 0
    var balls: Seq[Ball] = Seq.empty
    var springs: Seq[Spring] = Seq.empty
    var prevBall = Ball(ballMass, anchor = 1, x, 0)
    balls :+= prevBall
    (1 until 30).foreach { i =>
      var ball = Ball(ballMass, anchor = 0, x + (0.027 * i), -0.05 * i)
      balls :+= ball
      springs :+= Spring(ball, prevBall, springRestLength, springConst)
      prevBall = ball
    }

    Simulation(balls, springs)
  }

  private def screenHor(x: Double): Double = {
    iOrigin + (PixelsPerMeter * x);
  }

  private def screenVer(y: Double): Double =  {
    jOrigin - (PixelsPerMeter * y);
  }

  private def worldX(hor: Double): Double = {
    (hor - iOrigin) / PixelsPerMeter;
  }

  private def worldY(ver: Double): Double = {
    (jOrigin - ver) / PixelsPerMeter;
  }

  private def render(simulation: Simulation): Unit = {
    val canvas = canvasEl
    val context = canvas.getContext("2d")
    context.clearRect(0, 0, canvas.clientWidth, canvas.clientHeight);

    context.strokeStyle = "#03f"
    context.lineWidth = 1

    simulation.springs.foreach { s =>
      context.beginPath()
      context.moveTo(screenHor(s.ball1.x), screenVer(s.ball1.y))
      context.lineTo(screenHor(s.ball2.x), screenVer(s.ball2.y))
      context.stroke()
    }

    context.strokeStyle = "#000"
    context.lineWidth = 1;
    val pixelRadius = BallRadiusMeters * PixelsPerMeter
    simulation.balls.foreach { b =>
      if (b.anchor == 0) {
        // Draw each mobile ball as a filled-in circle.
        context.fillStyle = "#808080"
        context.beginPath()
        context.arc(screenHor(b.x), screenVer(b.y), pixelRadius, 0, 2 * math.Pi, true)
        context.fill()
        context.stroke()
      } else {
        // Draw each anchored ball as a filled-in square.
        context.fillStyle = "#FFD700"
        val x1 = screenHor(b.x) - pixelRadius
        val y1 = screenVer(b.y) - pixelRadius
        context.strokeRect(x1, y1, 2 * pixelRadius, 2 * pixelRadius)
        context.fillRect(x1, y1, 2 * pixelRadius, 2 * pixelRadius)
      }
    }
  }
}
