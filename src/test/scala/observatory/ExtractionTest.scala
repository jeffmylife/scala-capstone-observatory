package observatory

import java.time.LocalDate

import org.junit.Assert._
import org.junit.Test

trait ExtractionTest extends MilestoneSuite {
  private val milestoneTest = namedMilestoneTest("data extraction", 1) _

  def testTemps(year: Year): Unit = {

    val records = Extraction.locateTemperatures(year, "/stations.csv", "/" + year.toString + ".csv")
    val constraints = records.map { record =>
      val temperature:Temperature = record._3

      temperature > -273.15 &&
      temperature < 100.0
    }

    assert(constraints.forall(identity))
  }

  @Test def testAllTemps(): Unit = {
    val yearsToTest = 1975 to 1976

    yearsToTest.foreach(y => testTemps(y))
  }


}

