package observatory

import java.time.LocalDate

import scala.io.Source

/**
  * 1st milestone: data extraction
  */
object Extraction extends ExtractionInterface {

  def toCelsius(temperature: Temperature): Temperature = (temperature - 32) * 5 / 9.0

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
    val stationsStream = Source.fromInputStream(getClass.getResourceAsStream(stationsFile), "utf-8")

    val stations = stationsStream.getLines()
      .map(line => line.split(",", -1))
      .filter(line => line.count(_ != "") >= 3)
      .map { l =>
        List(l(0) + "-" + l(1),
          Location.apply(l(2).replace("+", "").toDouble,
            l(3).replace("+", "").toDouble
          )
        )
      }
      .map {
        case id :: loc :: Nil => id.asInstanceOf[String] -> loc.asInstanceOf[Location]
      }
      .toMap

    def getStationLocation(stn: String, wban: String): Location = (stn, wban)
      match {
        case (stn, wban) if stn != "" && wban != "" => stations.getOrElse(stn + "-" + wban, Location(0, 0))
        case (stn, wban) if wban != "" => stations.getOrElse("-" + wban, Location(0, 0))
        case (stn, wban) if stn != "" => stations.getOrElse(stn + "-", Location(0, 0))
      }

    val temperaturesStream = Source.fromInputStream(getClass.getResourceAsStream(temperaturesFile), "utf-8")
    val temperatures = temperaturesStream.getLines()
      .map(_.split(","))
      .filter(line => line.count(_ != "") >= 4)
      .map(_.toList)
      .map {
        case stn :: "" :: month :: day :: temp :: Nil => (stn, "", month, day, temp)
        case "" :: wban :: month :: day :: temp :: Nil => ("", wban, month, day, temp)
        case stn :: wban :: month :: day :: temp :: Nil => (stn, wban, month, day, temp)
      }
      .map { l =>
        (
          LocalDate.of(year.toInt, l._3.toInt, l._4.toInt),
          getStationLocation(l._1, l._2),
          toCelsius(l._5.toDouble)
        )
      }
      .filter(_._2 != Location(0, 0))
      .toSeq

    temperatures
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    val averages = records
      .map(record => (record._2, record._3))
      .groupBy(_._1)
      .map {
        case (key: Location, pairs) =>
          val temperatures: Iterable[Double] = pairs.map(_._2)
          key -> temperatures.sum / temperatures.size
      }
    averages.toSeq
  }

//  val records = locateTemperatures(1, "/stations.csv", "/1975.csv")
//  locationYearlyAverageRecords(records)
}
