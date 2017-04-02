package observatory


import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class VisualizationTest extends FunSuite with Checkers {


  test("Visualization should generate a image") {

    val temperatures = Extraction.locateTemperatures(2000, "/stations.csv",
      "/2000.csv")
    val average = Extraction.locationYearlyAverageRecords(temperatures)

    val colors = List((60.0, Color(255, 255, 255)),
      (32.0, Color(255, 0, 0)),
      (12.0, Color(255, 255, 0)),
      (0.0, Color(0, 255, 255)),
      (-15.0, Color(0, 0, 255)),
      (-27.0, Color(255, 0, 255)),
      (-50.0, Color(33, 0, 107)),
      (-60.0, Color(0, 0, 0)))
    val image = Visualization.visualize(average, colors)
    image.output("/home/weili/1.png")
  }

  test("my image test") {

    val colors = List((60.0, Color(255, 255, 255)),
      (32.0, Color(255, 0, 0)),
      (12.0, Color(255, 255, 0)),
      (0.0, Color(0, 255, 255)),
      (-15.0, Color(0, 0, 255)),
      (-27.0, Color(255, 0, 255)),
      (-50.0, Color(33, 0, 107)),
      (-60.0, Color(0, 0, 0)))
    val temperatures = List(
      (Location(90, -180), 60.0),
      (Location(90, 180), 60.0),
      (Location(-90, 180), 60.0),
      (Location(-90, -180), 60.0),
      (Location(0, 0), -20.0)
    )
    val width = 20
    val height = 10
    val calcTemperatures = for {
      y <- (0 until height).toArray
      x <- (0 until width).toArray
    } yield {
      val location = Visualization.locToLatLon(x, y)
      val temperature = Visualization.predictTemperature(temperatures, location)
      (x, y, temperature)
    }
    val calcColors = calcTemperatures.map(x => (x._1, x._2, Visualization.interpolateColor(colors, x._3)))

    val image = Visualization.visualize(temperatures, colors)
    image.output("/home/weili/2.png")
  }
}
