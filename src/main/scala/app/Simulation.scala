package app

import app.Simulation._

case class Simulation(balls: Seq[Ball], springs: Seq[Spring]) {

  // Initialize gravity vector: 9.8 m/s^2, pointing straight down.
  private val gravity = -9.8
  private var grabbedBall: Option[Ball] = None

  def update(dt: Double): Unit = {
    // Calculate the force vectors acting on all the balls:
    // both from springs and from gravity.

    // Start out with just the gravitational force on each ball
    balls.foreach { b =>
      b.fx = 0.0
      b.fy = b.mass * gravity
    }
    // Go through all the springs and calculate
    // the forces on the balls connected to their endpoints.
    // There will be equal and opposite forces on each pair.
    springs.foreach { s =>
      s.addForce()
    }

    // Now all the forces are correct.
    // Use the forces to update the position and speed of each ball.
    val friction = math.pow(0.5, dt / FrictionHalfLifeSeconds)

    balls.foreach { b =>
      // skip anchors, because they don't move
      if (b.anchor == 0) {

        // F = ma, therefore a = dv/dt = F/m.
        // dv = dt * F/m
        val dvx = dt * b.fx / b.mass;
        val dvy = dt * b.fy / b.mass;

        // Update the position using the mean speed in this increment.
        b.x += dt * (b.vx + dvx / 2.0)
        b.y += dt * (b.vy + dvy / 2.0)

        // Update the ball's speed. Apply friction to gradually reduce energy.
        b.vx = (friction * b.vx) + dvx;
        b.vy = (friction * b.vy) + dvy;
      }
    }
  }


  def grab(x: Int, y: Int): Unit = {
    // Not allowed to grab more than one ball at a time (safety valve).
    if (grabbedBall.isEmpty) {

      // Find the ball closest to the mouse coordinates.
      var closest: Ball = null
      var bestDistance: Double = Double.MaxValue;

      if (balls.nonEmpty) {
        var closest: Ball = balls.head
        var bestDistance = closest.distance(x, y)

        balls.tail.foreach { b =>
          val distance = b.distance(x, y)
          if (distance < bestDistance) {
            closest = b
            bestDistance = distance
          }
        }

        // If it is close enough to be grabbed, grab it.
        if (bestDistance <= GrabDistanceLimit) {
          closest.anchor += 1
          this.grabbedBall = Some(closest)
          pull(x, y)
        }
      }
    }
  }

  def pull(x: Double, y: Double): Unit = {
    grabbedBall.foreach { b =>
      b.x = x
      b.y = y
      b.vx = 0
      b.vy = 0
    }
  }

  def release(): Unit = {
    grabbedBall.foreach { b =>
      b.anchor -= 1
      grabbedBall = None
    }
  }

}


object Simulation {

  // Physics constants
  val SimStepsPerFrame: Int = 2000
  val FrameDelayMillis: Int = 10
  val FrictionHalfLifeSeconds: Double = 1.5
  val BallMass: Double = 0.1
  val SpringRestLength: Double = 0.04
  val SpringConst: Double = 500.0

  // Rendering constants
  val PixelsPerMeter = 200.0       // rendering zoom factor
  val iOrigin = 400                // hor location of world origin on canvas [pixels]
  val jOrigin = 100                // ver location of world origin on canvas [pixels]
  val BallRadiusMeters = 0.02
  val GrabDistanceLimit = 2.0
}