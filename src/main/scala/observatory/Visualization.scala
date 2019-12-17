package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 2nd milestone: basic visualization
  */
object Visualization extends VisualizationInterface with App {

  private def generateColors() = {
      """60 255 255 255
        |32 255	0	0
        |12	255	255	0
        |0	0	255	255
        |-15	0	0	255
        |-27	255	0	255
        |-50	33	0	107
        |-60	0	0	0
        |""".stripMargin.replaceAll("[^\\S\\r\\n]", " ").split('\n').map(_.trim)
        .map { row: String =>
          val splitted = row.split(' ').map(_.toInt)
          (splitted(0): Temperature, Color(splitted(1), splitted(2), splitted(3)))
        }
  }
  final val rgb_table = generateColors()




  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    val locationMatched = temperatures.filter(_._1 == location)
    if (locationMatched.size ==0) return -274
    if (locationMatched.size > 1) throw new Exception
   locationMatched.head._2
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {

    // Check upper and lower bounds
    val maxTemp = points.maxBy(_._1)
    if (value > maxTemp._1) return maxTemp._2

    val minTemp = points.minBy(_._1)
    if (value < minTemp._1) {
      if (value > -273.15) return maxTemp._2
      else throw new IllegalArgumentException(f"Below absolute zero using ${value}")
    }

    // Exact matches
    val matches = points.filter(_._1 == value)
    if (!matches.isEmpty) return matches.head._2


    // get temperature bounds for value
    val bounds = points
      .toArray
      .grouped(2)
      .find(pair => pair(0)._1 > value && pair(1)._1 < value)
      .get
      .map(_._1)
    if (bounds.size != 2) throw new Exception("Problem with color table")

    // colors at those bounds
    val p0:Color = points.find(_._1 == bounds(0)).head._2
    val p1:Color = points.find(_._1 == bounds(1)).head._2

    // line parameterization of temperature over color bounds
    val param = (t: Temperature) => {
      val diff = Seq(p0.red - p1.red, p0.green - p1.green, p0.blue - p1.blue)
        .map(_ * t / (bounds(0) + bounds(1)))
        .map(_.toInt)
      // add intercept
      Color(diff(0) + p1.red, diff(1) + p1.green, diff(2) + p1.blue)
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

