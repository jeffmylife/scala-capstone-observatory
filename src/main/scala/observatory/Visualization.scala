package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 2nd milestone: basic visualization
  */
object Visualization extends VisualizationInterface with App {


  def generateColors(string: String = TABLE1):Array[(Temperature, Color)] = {
      string.stripMargin.replaceAll("[^\\S\\r\\n]", " ").split('\n').map(_.trim)
        .map { row: String =>
          val splitted = row.split(' ').map(_.toDouble)
          (splitted(0): Temperature,
            Color(splitted(1).asInstanceOf[Int], splitted(2).asInstanceOf[Int], splitted(3).asInstanceOf[Int]))
        }
  }

  // kilometers
  final val EARTHS_RADIUS = 6378.137

  final val POWER = 2



  def ~=(x: Double, y: Double, precision: Double) = {
    if ((x - y).abs < precision) true else false
  }

  def ~=(location1: Location, location2: Location, precision: Double):Boolean = {
    if ((location1.lon - location2.lon).abs < precision
    && ((location1.lat - location2.lat).abs < precision)) true else false
  }


  def greatCircleDistance(location1: Location, location2: Location):Double = {
    // equidistant
    if (location1==location2 ||
      ~=(location1, location2, 10^(-9))) return 0.0

    // antipodal
    if (location1.lat == -1 * location2.lat
      && (~=(location1.lon, location2.lon + 180, 10^(-5))
      || ~=(location1.lon, location2.lon - 180, 10^(-5)))) return EARTHS_RADIUS *  Math.PI

    // otherwise
    val phi1 = location1.lat.toRadians
    val phi2 = location2.lat.toRadians
    val dLambda = (location1.lon.toRadians - location2.lon.toRadians).abs

    val dSigma = Math.acos(
      Math.sin(phi1) * Math.sin(phi2) +
        Math.cos(phi1) * Math.cos(phi2) * Math.cos(dLambda))

    // meters
    EARTHS_RADIUS * dSigma * 1000
  }


  def weightingFunctionIDW(l1: Location, l2: Location): Double = {
    val denom = Math.pow(greatCircleDistance(l1, l2), POWER)
    if (denom==0.0) Double.PositiveInfinity else 1 / denom
  }

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    // exact matches
    val locationMatched = temperatures.filter(t => t._1 == location || ~=(t._1,location, 10^(-9)))
    if (locationMatched.size > 1) throw new Exception
    if (locationMatched.nonEmpty) return locationMatched.head._2

    // inverse distance weighting
    val accumulators = temperatures.map{known =>
      val d = greatCircleDistance(known._1, location)
      if (d < 1000) return known._2
      val denom = Math.pow(d, POWER)
      val weight = if (denom==0.0) Double.PositiveInfinity else 1 / denom
      if (weight.isInfinity) return known._2
      (known._2, weight)
    }.foldLeft((0.0, 0.0)) {
      case ((accumulateWeights, accumulateProducts), (w, t)) => (accumulateWeights + w, accumulateProducts + t * w)
    }

    accumulators._2 / accumulators._1
  }
//   println(predictTemperature(Extraction
//    .locateTemperatures(1975, "/stations.csv", "/1975.csv").map(x => (x._2, x._3)),
//    Location(32.7157360, -117.1610870)))



  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {

    var sorted_points = points
      .toVector
      .sortBy(_._1)

    // Check upper and lower bounds
    val minTemp = sorted_points.head
    if (value < minTemp._1) {
      if (value > -273.15) return minTemp._2
      else throw new IllegalArgumentException(f"Below absolute zero using ${value}")
    }

    sorted_points = sorted_points.reverse

    // Upper bound
    val maxTemp = sorted_points.head
    if (value > maxTemp._1) return maxTemp._2

    // Exact matches
    val matches = sorted_points.filter(_._1 == value)
    if (!matches.isEmpty) return matches.head._2

    // find temperature bounds for value
    val finds = sorted_points
      .sliding(2, 1)
      .find(pair => pair(0)._1 > value && pair(1)._1 < value)

    // sanity check
    val tableString = sorted_points.mkString("\n\t")
    if (finds.isEmpty) throw new Exception(f"value = ${value} not found in table = \n\t${tableString}")

    val bounds = finds
      .get
      .map(_._1)
    if (bounds.size != 2) throw new Exception("Problem with color table")

    // determine vector direction  (p0 --> p1) based on initial ordering
    val isAscending = points.toSeq match {
      case Seq(_) => throw new Exception("Single-valued table")
      case a if (points.sliding(2, 1).forall { case Seq(x, y) => x._1 < y._1 }) => true
      case a if (points.sliding(2, 1).forall { case Seq(x, y) => x._1 > y._1 }) => false
      case _ => throw new Exception("Unordered table")
    }

    val head = isAscending

    // colors at those bounds
    val p1:Color = sorted_points.find(_._1 == bounds(if(head) 0 else 1)).head._2
    val p0:Color = sorted_points.find(_._1 == bounds(if(!head) 0 else 1)).head._2


    // line parameterization of temperature over color bounds
    val param = (t: Temperature) => {
      val diff = Seq(p1.red - p0.red, p1.green - p0.green, p1.blue - p0.blue)
          .map{
            // symmetric about 0
            case v if bounds.sum == 0 && bounds(0)!=0 =>  v * (t + bounds(0)) / (2 * bounds(0))
            // all other cases
            case v =>  v * (t - bounds(1)) / (bounds(0) - bounds(1))
            // should never happen
            case _ if bounds.sum == 0 && bounds(0)==0 => Double.PositiveInfinity
          }
      // add intercept
      Color((diff(0) + p0.red).round.toInt,
        (diff(1) + p0.green).round.toInt,
        (diff(2) + p0.blue).round.toInt)
    }
    param(value)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {

    ???
  }


}

