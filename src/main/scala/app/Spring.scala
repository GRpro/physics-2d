package app

case class Spring(ball1: Ball, ball2: Ball, restLength: Double, springConst: Double) {

  def addForce(): Unit = {
    // Calculate the length of the spring.
    val dx = ball2.x - ball1.x;
    val dy = ball2.y - ball1.y;
    val len = math.sqrt(dx * dx + dy * dy);

    // The difference between the spring's rest length and its current length
    // tells how much it is stretched or compressed.
    // Multiply by the spring constant to get the magnitude of the force.
    val displacement = len - restLength;
    val force = springConst * displacement;

    // Safety valve: if two balls are at the same location, we avoid division by zero.
    if (math.abs(len) >= 1.0e-6) {
      // Calculate force vector = force magnitude * directional unit vector
      val dfx = force * (dx / len);
      val dfy = force * (dy / len);

      // Add equal and opposite forces to the two connected balls.
      ball1.fx += dfx
      ball1.fy += dfy
      ball2.fx -= dfx
      ball2.fy -= dfy
    }
  }
}
