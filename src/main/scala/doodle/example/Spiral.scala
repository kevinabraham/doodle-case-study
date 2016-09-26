package doodle
package example

import doodle.core._
import doodle.backend.Canvas

object Spiral {

  def curve(t: Double): Vec = {
    // An interesting epicycloid. Changing the constants generates other
    // interesting shapes.
    val x = 100 * (Math.cos(t) + (Math.cos(7 * t) / 2) + (Math.sin(23 * t) / 3))
    val y = 100 * (Math.sin(t) + (Math.sin(7 * t) / 2) + (Math.cos(23 * t) / 3))
    Vec(x + 250, y + 250)
  }

  def draw(canvas: Canvas, end: Double = 10.0): Unit = {
    canvas.setSize(500, 500)
    canvas.setOrigin(-250, 200)
    canvas.beginPath()
    val Vec(x, y) = curve(0.0)
    canvas.moveTo(x, y)

    for(t <- 0.01 to end by 0.01) {
      val Vec(x,y) = curve(t)
      canvas.lineTo(x, y)
    }
    canvas.setStroke(Stroke(1.0, Color.darkBlue, Line.Cap.Round, Line.Join.Round))
    canvas.stroke()
  }
}
