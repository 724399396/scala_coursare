package observatory


import observatory.Visualization._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class VisualizationTest extends FunSuite with Checkers {

  test("temperatures predict") {
    val all = for {
      disA <- (0 to 100)
      disB <- (0 until disA)
    } yield {
      val compOp: ((Double, Double), (Double, Double)) => (Double, Double) = {
        case ((x1, y1), (x2, y2)) => (x1 + x2, y1 + y2)
      }
      val seqOp: ((Double, Double), (Double, Double)) => (Double, Double) = {
        case ((x, y), (dis, temp)) => (x + temp / dis, y + 1 / dis)
      }
      val predict = List((disA.toDouble, 10.0), (disB.toDouble, 20.0))
      (disA, disB, predict.find(_._1 == 0) match {
        case Some(x) =>
          x._2
        case None =>
          val (weightedTemp, weighted) = predict.aggregate((0.0, 0.0))(seqOp, compOp)
          if (weighted == 0.0) {
            weightedTemp
          } else {
            weightedTemp / weighted
          }
      })
    }
    assert(all.filter(_._3 < 15).size == 0)
  }

  test("Visualization should generate a image") {
    val temperatures = Extraction.locateTemperatures(2015, "/stations.csv",
      "/2015.csv")
    val average = Extraction.locationYearlyAverageRecords(temperatures)

    val colors = List((60.0, Color(255, 255, 255)),
      (32.0, Color(255, 0, 0)),
      (12.0, Color(255, 255, 0)),
      (0.0, Color(0, 255, 255)),
      (-15.0, Color(0, 0, 255)),
      (-27.0, Color(255, 0, 255)),
      (-50.0, Color(33, 0, 107)),
      (-60.0, Color(0, 0, 0)))
    val image = visualize(average, colors)
    image.output("d:/1.png")
  }
}
