package observatory

import java.time.LocalDate

import scala.io.Source

/**
  * 1st milestone: data extraction
  */
object Extraction {

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {
    val locMap = Source.fromInputStream(getClass.getResourceAsStream(stationsFile)).getLines()
    .map(_.split(",", 4)).filter{
      case Array(_,_,latitude,longitude) =>
        !latitude.isEmpty && !longitude.isEmpty
    }.map {
      case Array(stn,wban,latitude,longitude) =>
        val loc = Location(latitude.toDouble, longitude.toDouble)
        ((stn,wban), loc)
    }.toMap

    Source.fromInputStream(getClass.getResourceAsStream(temperaturesFile)).getLines()
      .map(_.split(",", 5)).filter{
      case Array(_, _, _, _, temperature) =>
        temperature.toDouble != 9999.9
    }.map {
      case Array(stn, wban, month, day, temperature) =>
        val date = LocalDate.of(year, month.toInt, day.toInt)
        val locOpt = locMap.get((stn, wban))
        locOpt.map(loc => (date, loc, (temperature.toDouble - 32) / 1.8))
    }.filter(_.isDefined).map(_.get).toIterable
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    records.groupBy(_._2).mapValues(vs => vs.map(_._3).sum / vs.size)
  }
}
