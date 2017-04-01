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

    val colors = List( (60.0, Color(255,255,255)),
      (32.0, Color(255,0,0)),
      (12.0, Color(255,255,0)),
      (0.0,Color(0,255,255)),
      (-15.0,Color(0,0,255)),
      (-27.0,Color(255,0,255)),
      (-50.0,Color(33,0,107)),
      (-60.0,Color(0,0,0)))
    val image = Visualization.visualize(average, colors)
    image.output("/home/liwwli/1.png")
  }
}
