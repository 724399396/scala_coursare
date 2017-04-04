package observatory

import java.lang.Math._

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 2nd milestone: basic visualization
  */
object Visualization {
  private val earthRadius = 6371

  private def time[A](a: => A): A = {
    val start = System.nanoTime()
    val res = a
    val end = System.nanoTime()
    val elapsed = end - start
    res
  }

  def distance(locA: Location, locB: Location): Double = {
    val Location(latA, lonA) = locA
    val Location(latB, lonB) = locB
    val latDistance = toRadians(latB - latA)
    val lonDistance = toRadians(lonB - lonA)

    val a = pow(sin(latDistance / 2), 2) +
    cos(toRadians(latA)) * cos(toRadians(latB)) *
    pow(sin(lonDistance / 2), 2)

    val c = 2 * atan2(sqrt(a), sqrt(1 - a))
    c * earthRadius
  }

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location     Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = {
    val compOp: ((Double, Double), (Double, Double)) => (Double, Double) = {
      case ((x1, y1), (x2, y2)) => (x1 + x2, y1 + y2)
    }
    val seqOp: ((Double, Double), (Double, Double)) => (Double, Double) = {
      case ((x, y), (dis, temp)) => (x + temp / dis, y + 1 / dis)
    }
    val predicts = temperatures.map {
      case (loc, temperature) => (distance(loc, location), temperature)
    }
    predicts.find(_._1 == 0) match {
      case Some(x) =>
        x._2
      case None =>
        val (weightedTemp, weighted) = predicts.aggregate((0.0, 0.0))(seqOp, compOp)
        if (weighted == 0.0) {
          weightedTemp
        } else {
          weightedTemp / weighted
        }
    }
  }

  private def linearInterpolation(key1: Double, key0: Double, key: Double)
                                 (val1: Double, val0: Double): Double = {
    val0 + (key - key0) * (val1 - val0) / (key1 - key0)
  }

  private def linearColor(point1: (Double, Color), point2: (Double, Color),
                          value: Double): Color = {
    val (k1, Color(r1, g1, b1)) = point1
    val (k2, Color(r2, g2, b2)) = point2
    val func = linearInterpolation(k1, k2, value) _
    val r = func(r1, r2).round.toInt
    val g = func(g1, g2).round.toInt
    val b = func(b1, b2).round.toInt
    Color(r, g, b)
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value  The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color = {
    points.find(_._1 == value) match {
      case Some(x) => x._2
      case None =>
        val point1Opt = points.filter(_._1 > value) match {
          case Nil => None
          case x => Some(x.minBy(_._1))
        }
        val point2Opt = points.filter(_._1 < value) match {
          case Nil => None
          case x => Some(x.maxBy(_._1))
        }
        val colorOpt = for {
          point1 <- point1Opt
          point2 <- point2Opt
        } yield {
          linearColor(point1, point2, value)
        }
        colorOpt.getOrElse(points.minBy(x => abs(value - x._1))._2)
    }
  }

  private def locToLatLon(x: Int, y: Int): Location = {
    Location(90 - y, x - 180)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
    val width = 360
    val height = 180
    val predictTemperatures = for {
      y <- (0 until height).toArray
      x <- (0 until width).toArray
    } yield {
      val location = locToLatLon(x, y)
      predictTemperature(temperatures, location)
    }

    val predictColors = predictTemperatures.map(interpolateColor(colors, _))
    val pixels = predictColors.map{
      case Color(r, g, b) => Pixel(r, g, b, 255)
    }

    Image(width, height, pixels)
  }

}

