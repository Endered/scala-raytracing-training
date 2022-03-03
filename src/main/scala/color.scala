package Color
import Util.Util
import Util.{clamp}

case class Color(val r: Double, val g: Double, val b: Double) {

  private def toInt(x: Double): Int =
    clamp[Int]((255.999 * Math.sqrt(x)).toInt, 0, 255)
  override def toString(): String = s"${toInt(r)} ${toInt(g)} ${toInt(b)}"

  def +(that: Color): Color = Color(r + that.r, g + that.g, b + that.b)
  def -(that: Color): Color = Color(r - that.r, g - that.g, b - that.b)
  def *(that: Color): Color = Color(r * that.r, g * that.g, b * that.b)
  def /(that: Color): Color = Color(r / that.r, g / that.g, b / that.b)

  def *(that: Double): Color = Color(r * that, g * that, b * that)
  def /(that: Double): Color = Color(r / that, g / that, b / that)

  def unary_- = Color(-r, -g, -b)

  def length(): Double = Math.sqrt(lengthSquare())
  def lengthSquare(): Double = r * r + g * g + b * b

  def unit: Color = this / this.length()
}

object Color {
  def apply(x: Double, y: Double, z: Double): Color = new Color(x, y, z)
  val Zero = Color(0, 0, 0)

  implicit class NumberIsColorMultiplier(val k: Double) {
    def *(that: Color): Color = that * k
  }

  def random(min: Double, max: Double): Color =
    Color(Util.random(min, max), Util.random(min, max), Util.random(min, max))
  def random(): Color = random(0, 1)

}
