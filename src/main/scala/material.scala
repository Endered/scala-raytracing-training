package Material
import Hit.{HitRecord}
import Geometry.{Ray, Vector3}
import Color.Color
import Vector3.{dot, reflect, randomUnitVector, refract}
import Util.Util.random

trait Material {
  def scatter(ray: Ray, record: HitRecord): Option[(Color, Ray)]
}

class Lambertian(albedo: Color) extends Material {
  def scatter(ray: Ray, record: HitRecord): Option[(Color, Ray)] = {
    val scatterDirection = record.normal + randomUnitVector()
    val direction =
      if (scatterDirection.nearZero) record.normal else scatterDirection
    Some((albedo, Ray(record.p, direction)))
  }
}

object Lambertian {
  def apply(albedo: Color): Lambertian = new Lambertian(albedo)
}

class Metal(albedo: Color, fuzz: Double) extends Material {
  private val f = if (fuzz < 1) fuzz else 1
  def scatter(ray: Ray, record: HitRecord): Option[(Color, Ray)] = {
    val reflected =
      reflect(ray.direction.unit, record.normal) + f * randomUnitVector()
    if (dot(reflected, record.normal) > 0)
      Some((albedo, Ray(record.p, reflected)))
    else None
  }
}

object Metal {
  def apply(albedo: Color, fuzz: Double): Metal = new Metal(albedo, fuzz)
}

class Dialectric(indexOfRefraction: Double) extends Material {

  private def reflectance(cosine: Double, refIdx: Double): Double = {
    val tmp = (1 - refIdx) / (1 + refIdx)
    val r0 = tmp * tmp
    r0 + (1 - r0) * Math.pow(1 - cosine, 5)
  }

  def scatter(ray: Ray, record: HitRecord): Option[(Color, Ray)] = {
    val refractionRatio =
      if (record.frontFace) (1.0 / indexOfRefraction) else indexOfRefraction
    val unitDirection = ray.direction.unit
    val cosTheta = Math.min(dot(-unitDirection, record.normal), 1.0)
    val sinTheta = Math.sqrt(1 - cosTheta * cosTheta)
    val direction =
      if (
        refractionRatio * sinTheta > 1.0 ||
        reflectance(cosTheta, refractionRatio) > random()
      ) reflect(unitDirection, record.normal)
      else refract(unitDirection, record.normal, refractionRatio)
    Some(
      (
        Color(1.0, 1.0, 1.0),
        Ray(
          record.p,
          direction
        )
      )
    )
  }
}

object Dialectric {
  def apply(indexOfRefraction: Double): Dialectric = new Dialectric(
    indexOfRefraction
  )
}
