package spp

import org.scalatest._
import spp.utils._
import spp.utils.Pipeline
import java.io.File
import scala.io.Source

abstract class OutputComparisonSpec extends FlatSpec {
  val inputExtension: String
  val outputExtension: String
  val pipeline: Pipeline[File, String]

  val rootDir = "src/test/resources/"
  val rootInput = rootDir + "input/"
  val rootOutput = rootDir + "output/"

  def outputMatch(testName: String): Assertion = {
    val expected = Source.fromFile(
        new File(rootOutput + testName + outputExtension)
    ).getLines().mkString

    val inputFileName = rootInput + testName + inputExtension

    val context = Context(
        new utils.Reporter, List(inputFileName)
    )
    val actual: String = pipeline.run(context)(new File(inputFileName))

    assertResult(expected)(actual)
  }
}
