package doodle.core

import doodle.backend.Canvas

/**
 * Created by kabraham16 on 9/27/16.
 */
sealed trait Image {
  val boundingBox: BoundingBox = {
    this match {
      case Circle(r) => BoundingBox(-r, r, r, -r)
      case Rectangle(w, h) => BoundingBox(-w / 2, h / 2, w / 2, -h / 2)
      case On(a, b) => a.boundingBox on b.boundingBox
      case Above(a, b) => a.boundingBox above b.boundingBox
      case Beside(a, b) => a.boundingBox beside b.boundingBox
    }
  }

  def on(image: Image): Image = On(this, image)

  def above(image: Image): Image = Above(this, image)

  def beside(image: Image): Image = Beside(this, image)

  def draw(canvas: Canvas): Unit = draw(canvas, 0.0, 0.0)

  def draw(canvas: Canvas, originX: Double, originY: Double): Unit = {
    this match {
      case Circle(r) => canvas.circle(0.0, 0.0, r)
      case Rectangle(w, h) => canvas.rectangle(-w / 2, h / 2, w / 2, -h / 2)
      case On(o, u) =>
        o.draw(canvas, originX, originY)
        u.draw(canvas, originX, originY)
      case Above(a, b) =>
        val box = this.boundingBox
        val aBox = a.boundingBox
        val bBox = b.boundingBox

        val aboveOriginY = originY + box.top - (aBox.height / 2)
        val belowOriginY = originY + box.bottom + (bBox.height / 2)

        a.draw(canvas, originX, originY)
        b.draw(canvas, originX, originY)
      case Beside(l, r) =>
        val box = this.boundingBox
        val lBox = l.boundingBox
        val rBox = r.boundingBox

        val leftOriginX = originX + box.left + (lBox.width / 2)
        val rightOriginX = originX + box.right - (rBox.width / 2)

        l.draw(canvas, leftOriginX, originY)
        r.draw(canvas, rightOriginX, originY)
    }
  }
}

final case class Circle(radius: Double) extends Image

final case class Rectangle(width: Double, height: Double) extends Image

final case class On(on: Image, under: Image) extends Image

final case class Above(above: Image, below: Image) extends Image

final case class Beside(left: Image, right: Image) extends Image
