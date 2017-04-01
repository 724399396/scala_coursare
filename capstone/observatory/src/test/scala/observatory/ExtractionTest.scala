package observatory

import java.time.LocalDate

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ExtractionTest extends FunSuite {

  test("2000 should have special")  {
    val temperatures = Extraction.locateTemperatures(2000, "/stations.csv",
      "/2000.csv")

    val (hLocalDate, hLoc, hTemp) = temperatures.head
    assert(hLocalDate === LocalDate.of(2000, 1, 1))
    assert(hLoc === Location(70.933,-008.667))
    assert(hTemp === 31.4)

    val average = Extraction.locationYearlyAverageRecords(temperatures)

    println(average.find(_._1 == Location(1.0,-1.0)))
    println(average)
  }
}