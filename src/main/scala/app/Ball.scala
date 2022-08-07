package app

case class Ball(mass: Double,
                var anchor: Int, // 0=mobile, positive=fixed in place
                var x: Double, var y: Double, // position vector
                var vx: Double = 0, var vy: Double = 0, // velocity vector
                var fx: Double = 0, var fy: Double = 0 // force vector
               ) {

  def distance(x: Double, y: Double): Double = {
    val dx = this.x - x
    val dy = this.y - y
    math.sqrt(dx * dx + dy * dy)
  }
}
