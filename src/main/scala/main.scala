import Geometry.{Vector3, Ray}
import Color._
import Hit.{Hittable, HitRecord, Sphere}
import Camera.Camera
import Util.Util.random
import Material.{Lambertian, Metal, Dialectric}
import scala.collection.immutable.Vector
import scala.collection.parallel.CollectionConverters._

object Run {
  def rayColor(ray: Ray, world: Hittable, depth: Int): Color = {
    if (depth <= 0) Color(0, 0, 0)
    else
      world
        .hit(ray, 0.001, Double.MaxValue)
        .map {
          case (record @ HitRecord(_, _, material, _, _)) => {
            material
              .scatter(ray, record)
              .map {
                case (attenuation, scatterd) => {
                  attenuation * rayColor(scatterd, world, depth - 1)
                }
              }
              .getOrElse(Color(0, 0, 0))
          }
        }
        .getOrElse {
          val unitDirection = ray.direction.unit
          val t = 0.5 * (unitDirection.y + 1.0)
          Color.lerp(Color(1.0, 1.0, 1.0), Color(0.5, 0.7, 1.0), t)
        }
  }
  def randomScene(): Iterable[Hittable] = {
    val materialGround = Lambertian(Color(0.5, 0.5, 0.5))
    var res = Vector.empty[Hittable]
    res :+= Sphere(Vector3(0, -1000, 0), 1000, materialGround)
    for {
      a <- -11 until 11;
      b <- -11 until 11
    } {
      val center = Vector3(a + 0.9 * random(), 0.2, b + random())
      val chooseMat = random()
      val material = if (chooseMat < 0.8) {
        val albedo = Color.random() * Color.random()
        Lambertian(albedo)
      } else if (chooseMat < 0.95) {
        val albedo = Color.random(0.5, 1)
        val fuzz = random(0, 0.5)
        Metal(albedo, fuzz)
      } else {
        Dialectric(1.5)
      }
      res :+= Sphere(center, 0.2, material)
    }
    res
  }
}

object Main extends App {
  val aspectRatio: Double = 3.0 / 2.0
  val imageWidth = 1200
  val imageHeight = (imageWidth / aspectRatio).toInt
  val samplesPerPixel = 500
  val maxDepth = 50

  println(s"P3\n$imageWidth $imageHeight\n255")

  val lookFrom = Vector3(13, 2, 3)
  val lookAt = Vector3(0, 0, 0)
  val vup = Vector3(0, 1, 0)
  val distToFocus = 10
  val aperture = 0.1

  val camera = Camera(
    lookFrom,
    lookAt,
    vup,
    20,
    aspectRatio,
    aperture,
    distToFocus
  )

  val world = Run.randomScene()

  val points = for {
    j <- (imageHeight - 1) to 0 by -1;
    i <- 0 until imageWidth
  } yield (i, j)
  val pointsNumber = points.length

  val printProgress = {
    var count = 0
    var last = -1
    ({ () =>
      count += 1
      val progress = (count.toDouble / pointsNumber * 100).toInt
      if (last < progress) {
        System.err.print(f"\rProgress ${progress}%3d%%")
        last = progress
      }
    })
  }
  points.zipWithIndex.par
    .map {
      case ((i, j), count) => {
        printProgress()
        (1 to samplesPerPixel)
          .map { _ =>
            val u = (i.toDouble + random()).toDouble / (imageWidth - 1)
            val v = (j.toDouble + random()).toDouble / (imageHeight - 1)
            val ray = camera.getRay(u, v)
            Run.rayColor(ray, world, maxDepth)
          }
          .reduce(_ + _) / samplesPerPixel
      }
    }
    .toList
    .foreach(println)
  System.err.println("\nDone")
}
