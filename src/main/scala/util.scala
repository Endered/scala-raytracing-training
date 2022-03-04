package Util
import scala.util.Random
import Geometry.{Vector3}
import Color.Color

object Util {
  def random(low: Double, high: Double): Double =
    low + (high - low) * Random.nextDouble()
  def random(): Double = random(0, 1)

  def min[T](x: T, y: T)(implicit num: Numeric[T]): T = {
    import num._
    if (x < y) x else y
  }
  def max[T](x: T, y: T)(implicit num: Numeric[T]): T = {
    import num._
    if (x > y) x else y
  }

  def clamp[T](x: T, low: T, high: T)(implicit
      num: Numeric[T]
  ): T = {
    max(min(x, high), low)
  }
}
