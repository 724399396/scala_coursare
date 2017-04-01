package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 2nd milestone: basic visualization
  */
object Visualization {
  private val earthRadius = 6378.137

  private def time[A](a: => A):A = {
    val start = System.nanoTime()
    val res = a
    println(System.nanoTime() - start)
    res
  }

  private def rad(d: Double): Double = {
    Math.PI * d / 180
  }

  import collection.mutable.{ Map => MutableMap }
  val sinMap: MutableMap[Double,Double] = MutableMap()
  private def cacheSin(in: Double): Double = {
    sinMap.get(in) match {
      case None =>
        val res = Math.sin(in)
        sinMap += (in -> res)
        res
      case Some(x) =>
        x
    }
  }

  val cosMap: MutableMap[Double,Double] = MutableMap()
  private def cacheCos(in: Double): Double = {
    sinMap.get(in) match {
      case None =>
        val res = Math.cos(in)
        sinMap += (in -> res)
        res
      case Some(x) =>
        x
    }
  }

  val disMap: MutableMap[(Location,Location), Double] = MutableMap()
  def distance(locA: Location, locB: Location): Double = {
    val key = if (locA.lat < locB.lat) (locA, locB) else (locB, locA)
    disMap.get(key) match {
      case None =>
        val radLat1 = rad(locA.lat)
        val radLat2 = rad(locB.lat)
        val a = radLat1 - radLat2
        val b = rad(locA.lon) - rad(locB.lon)
        val s = 2 * Math.asin(Math.sqrt(Math.pow(cacheSin(a / 2), 2) +
          cacheCos(radLat1) * cacheCos(radLat2) *
            Math.pow(cacheSin(b / 2), 2)))
        val res = s * earthRadius
        disMap(key) = res
        res
      case Some(x) =>
        x
    }
  }

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = {
    val (weightedTemp, weighted) = temperatures.map {
      case (loc, temperature) => (distance(loc, location), temperature)
    }.toList.sortBy(_._1).take(1000).foldLeft((0.0,0.0)) {
      case ((x, y), (dis,temp)) =>
        (x + temp/dis, y + 1/dis)
    }
    weightedTemp / weighted
  }

  private def linearInterpolation(key1: Double, key0: Double, key: Double)
                                 (val1: Double, val0: Double): Double = {
    val0 + (key - key0) * (val1 - val0) / (key1 - key0)
  }

  private def linearColor(point1: (Double, Color), point2: (Double, Color),
                         value: Double): Color = {
    val (k1, Color(r1,g1,b1)) = point1
    val (k2, Color(r2,g2,b2)) = point2
    val func = linearInterpolation(k1,k2, value) _
    val r = func(r1,r2).round.toInt
    val g = func(g1,g2).round.toInt
    val b = func(b1,b2).round.toInt
    Color(r,g,b)
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
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
        colorOpt.getOrElse(points.minBy(x => Math.abs(value - x._1))._2)
    }
  }

  private def locToLatLon(x: Int, y: Int): Location = {
    Location(90 - y, x - 180)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
    val width = 360
    val height = 180
    val pixels = for {
      y <- (0 until height)
      x <- (0 until width)
    } yield {
      val location = locToLatLon(x, y)
      val temperature = predictTemperature(temperatures, location)
      val Color(r, g, b) = interpolateColor(colors, temperature)
      Pixel(r, g, b, 255)
    }

    Image(width, height, pixels.toArray)
  }

}
