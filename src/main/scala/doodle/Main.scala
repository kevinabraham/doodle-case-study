package doodle

object Main extends App {
  import doodle.core._
  import doodle.jvm.Java2DCanvas
  import doodle.example._
  import javax.swing.JFrame

  val canvas = Java2DCanvas.canvas
  canvas.panel.frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)

  Spiral.draw(canvas)
}
