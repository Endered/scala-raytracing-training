package Geometry
import Util.Util

case class Vector3(val x: Double, val y: Double, val z: Double) {
  override def toString(): String = s"$x $y $z"

  def +(that: Vector3): Vector3 = Vector3(x + that.x, y + that.y, z + that.z)
  def -(that: Vector3): Vector3 = Vector3(x - that.x, y - that.y, z - that.z)
  def *(that: Vector3): Vector3 = Vector3(x * that.x, y * that.y, z * that.z)
  def /(that: Vector3): Vector3 = Vector3(x / that.x, y / that.y, z / that.z)

  def *(that: Double): Vector3 = Vector3(x * that, y * that, z * that)
  def /(that: Double): Vector3 = Vector3(x / that, y / that, z / that)

  def unary_- = Vector3(-x, -y, -z)

  def length(): Double = Math.sqrt(lengthSquare())
  def lengthSquare(): Double = x * x + y * y + z * z

  def unit: Vector3 = this / this.length()

  def nearZero: Boolean = length() < Vector3.EPS
}

object Vector3 {
  val EPS = 1e-8

  def apply(x: Double, y: Double, z: Double): Vector3 = new Vector3(x, y, z)
  val Zero = Vector3(0, 0, 0)

  implicit class NumberIsVector3Multiplier(val k: Double) {
    def *(that: Vector3): Vector3 = that * k
  }

  def dot(a: Vector3, b: Vector3): Double = a.x * b.x + a.y * b.y + a.z * b.z
  def cross(a: Vector3, b: Vector3): Vector3 =
    Vector3(a.y * b.z - a.z * b.y, a.z * b.x - a.x * b.z, a.x * b.y - a.y * b.x)
  def reflect(v: Vector3, n: Vector3): Vector3 = v - 2 * dot(v, n) * n
  def refract(uv: Vector3, n: Vector3, etaiOverEtat: Double): Vector3 = {
    val cosTheta = Math.min(dot(-uv, n), 1.0)
    val rOutPerp = etaiOverEtat * (uv + cosTheta * n)
    val rOutParallel = -Math.sqrt(Math.abs(1.0 - rOutPerp.lengthSquare())) * n
    rOutPerp + rOutParallel
  }

  def random(min: Double, max: Double): Vector3 =
    Vector3(Util.random(min, max), Util.random(min, max), Util.random(min, max))
  def random(): Vector3 = random(0, 1)

  def randomInUnitSphere(): Vector3 = {
    val v = random()
    if (v.lengthSquare() >= 1) randomInUnitSphere()
    else v
  }
  def randomUnitVector(): Vector3 = randomInUnitSphere().unit
  def randomInHemisphere(normal: Vector3): Vector3 = {
    val v = randomUnitVector()
    if (dot(v, normal) > 0) v
    else -v
  }
  def randomInUnitDisk(): Vector3 = {
    val p = Vector3(Util.random(-1, 1), Util.random(-1, 1), 0)
    if (p.lengthSquare() > 1) randomInUnitDisk() else p
  }
}

case class Ray(original: Vector3, direction: Vector3) {
  def at(time: Double): Vector3 = original + direction * time
}

object Ray {
  def apply(original: Vector3, direction: Vector3): Ray =
    new Ray(original, direction)
}
