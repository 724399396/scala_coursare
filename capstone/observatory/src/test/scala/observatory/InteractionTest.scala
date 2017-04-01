package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

import scala.collection.concurrent.TrieMap

@RunWith(classOf[JUnitRunner])
class InteractionTest extends FunSuite with Checkers {
  test("test generate tile") {
    val temperatures = Extraction.locateTemperatures(2015, "/stations.csv",
      "/2015.csv")
    val average = Extraction.locationYearlyAverageRecords(temperatures)

    val colors = List( (60.0, Color(255,255,255)),
      (32.0, Color(255,0,0)),
      (12.0, Color(255,255,0)),
      (0.0,Color(0,255,255)),
      (-15.0,Color(0,0,255)),
      (-27.0,Color(255,0,255)),
      (-50.0,Color(33,0,107)),
      (-60.0,Color(0,0,0)))
    for {
      zoom <- 0 to 3
      n = 2 ^ zoom
      x <- 0 until n
      y <- 0 until n
    } {
      val image = Interaction.tile(average, colors, zoom, x, y)
      image.output(getClass.getResource(s"/temperatures/2015/$zoom/$x-$y.png").getFile)
    }
  }
}
