package doodle.core

/**
 * Created by kabraham16 on 9/27/16.
 */
final case class BoundingBox(left: Double, top: Double, right: Double, bottom: Double) {
  val height: Double = top - bottom
  val width: Double = right - left

  def above(that: BoundingBox): BoundingBox =
    BoundingBox(this.left min that.left, (this.height + that.height) / 2,
      this.right max that.right, -(this.height + that.height) / 2)

  def beside(that: BoundingBox): BoundingBox =
    BoundingBox(-(this.width + that.width) / 2, this.top max that.top,
      (this.width + that.width) / 2, this.bottom min that.bottom)

  def on(that: BoundingBox): BoundingBox =
    BoundingBox(this.left min that.left, this.top max that.top,
      this.right max that.right, this.bottom min that.bottom)
}
