package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import Math._

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  /**
    * @param zoom Zoom level
    * @param x X coordinate
    * @param y Y coordinate
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(zoom: Int, x: Int, y: Int): Location = {
    val n = 2 ^ zoom
    val lonDeg = x / n * 360.0 - 180.0
    val latRad = atan(sinh(PI * (1 - 2 * y / n)))
    val latDeg = toDegrees(latRad)
    Location(latDeg, lonDeg)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param zoom Zoom level
    * @param x X coordinate
    * @param y Y coordinate
    * @return A 256×256 image showing the contents of the tile defined by `x`, `y` and `zooms`
    */
  def tile(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)], zoom: Int, x: Int, y: Int): Image = {
    val img = Image(256, 256)
    val Location(lat, lon) = tileLocation(zoom, x, y)
    val n = 2 ^ zoom
    val latStep = (360.0 / n) / 256
    val lonStep = (180.0 / n) / 256
    for {
      cx <- (0 until 255).toArray
      cy <- (0 until 255).toArray
    } {
      println((zoom, x, y, cx, cy))
      val location = Location(lat + latStep * cx, lon + lonStep * cy)
      val temperature = Visualization.predictTemperature(temperatures, location)
      val Color(r, g, b) = Visualization.interpolateColor(colors, temperature)
      img.setPixel(cx, cy, Pixel(r, g, b, 127))
    }

    img
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Int, Data)],
    generateImage: (Int, Int, Int, Int, Data) => Unit
  ): Unit = {
    ???
  }

}
