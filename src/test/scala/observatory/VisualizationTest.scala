package observatory

import org.junit.Assert._
import org.junit.Test


trait VisualizationTest extends MilestoneSuite {
  private val milestoneTest = namedMilestoneTest("raw data display", 2) _

  // Implement tests for the methods of the `Visualization` object



  @Test def interpolateColorTest(): Unit = {
    var result = Visualization.interpolateColor(Visualization.generateColors(), 6)
    var expected = Color(128, 255, 128)
    assert(result == expected,
    s"Expected:<${expected}> but was:<${result}>")
    println(result)
    println("_____________")

    result = Visualization.interpolateColor(Visualization.generateColors(TABLE3), -0.75)
    expected = Color(191,0,64)
    assert(result == expected,
      s"Expected:<${expected}> but was:<${result}>")
    println(result)
    println("_____________")

    result = Visualization.interpolateColor(Visualization.generateColors(TABLE4), -0.5)
    expected = Color(191,0,64)
    assert(result == expected,
      s"Expected:<${expected}> but was:<${result}>")
    println(result)
    println("_____________")


    result = Visualization.interpolateColor(Visualization.generateColors(TABLE6), 1.75)
    expected = Color(191,0,64)
    assert(result == expected,
      s"Expected:<${expected}> but was:<${result}>")
    println(result)
    println("_____________")

    result = Visualization.interpolateColor(Visualization.generateColors(TABLE7), -2.0)
    expected = Color(191,0,64)
    assert(result == expected,
      s"Expected:<${expected}> but was:<${result}>")
    println(result)
    println("_____________")
  }

}
