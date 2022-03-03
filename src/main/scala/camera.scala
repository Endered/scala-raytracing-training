package Camera

import Geometry.{Vector3, Ray}
import Vector3._

class Camera(
    lookFrom: Vector3,
    lookAt: Vector3,
    vup: Vector3,
    vfov: Double,
    aspectRatio: Double,
    aperture: Double,
    focusDist: Double
) {
  val theta = vfov.toRadians
  val h = Math.tan(theta / 2)
  val viewportHeight = 2.0 * h
  val viewportWidth = aspectRatio * viewportHeight

  val w = (lookFrom - lookAt).unit
  val u = cross(vup, w).unit
  val v = cross(w, u)

  val origin = lookFrom
  val horizontal = focusDist * viewportWidth * u
  val vertical = focusDist * viewportHeight * v
  val lowerLeftCorner = origin - horizontal / 2 - vertical / 2 - focusDist * w
  val lensRadius = aperture / 2

  def getRay(s: Double, t: Double): Ray = {
    val rd = lensRadius * randomInUnitDisk()
    val offset = u * rd.x + v * rd.y
    Ray(
      origin + offset,
      lowerLeftCorner + s * horizontal + t * vertical - origin - offset
    )
  }
}

object Camera {
  def apply(
      lookFrom: Vector3,
      lookAt: Vector3,
      vup: Vector3,
      vfov: Double,
      aspectRatio: Double,
      aperture: Double,
      focusDist: Double
  ): Camera =
    new Camera(lookFrom, lookAt, vup, vfov, aspectRatio, aperture, focusDist)
}
