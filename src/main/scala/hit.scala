package Hit

import Geometry._
import Vector3._
import Material.Material

object Hit

case class HitRecord(
    p: Vector3,
    normal: Vector3,
    material: Material,
    time: Double,
    frontFace: Boolean
)

object HitRecord {
  def apply(
      p: Vector3,
      normal: Vector3,
      material: Material,
      time: Double,
      frontFace: Boolean
  ): HitRecord =
    new HitRecord(p, normal, material, time, frontFace)
}

trait Hittable {
  def hit(ray: Ray, tMin: Double, tMax: Double): Option[HitRecord]
}

object Hittable {
  implicit class IterableIsHittable[CC[X] <: Iterable[X], T <: Hittable](
      itr: CC[T]
  ) extends Hittable {
    def hit(ray: Ray, tMin: Double, tMax: Double): Option[HitRecord] = {
      itr
        .flatMap(_.hit(ray, tMin, tMax))
        .toList match {
        case Nil => None
        case v   => Some(v.minBy(_.time))
      }
    }
  }
}

case class Sphere(center: Vector3, radius: Double, material: Material)
    extends Hittable {
  def hit(ray: Ray, tMin: Double, tMax: Double): Option[HitRecord] = {
    val Ray(origin, direction) = ray
    val oc = origin - center
    val a = direction.lengthSquare()
    val b = dot(oc, direction)
    val c = oc.lengthSquare() - radius * radius
    val disciminant = b * b - a * c
    if (disciminant < 0) None
    else {
      val sqrtd = Math.sqrt(disciminant)
      List((-b - sqrtd) / a, (-b + sqrtd) / a)
        .find { v: Double => tMin < v && v < tMax }
        .map { root =>
          val p = ray.at(root)
          val normal = (p - center) / radius
          val (n, outward) =
            if (dot(direction, normal) < 0) (normal, true) else (-normal, false)
          HitRecord(p, n, material, root, outward)
        }
    }
  }
}

object Sphere {
  def apply(center: Vector3, radius: Double, material: Material): Sphere =
    new Sphere(center, radius, material)
}
