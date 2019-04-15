package notes
import minitest._
import notes.ObjectTestSuite.{assertEquals, testAsync}
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object ChurchEncodingTest extends SimpleTestSuite {
  import ChurchEncoding._
  test("GreaterThan and LessThan true") {
    assert(!program(Interpreter))
  }
  testAsync("GreaterThan and LessThan Async true") {
    val future = program(AsyncInterpreter)

    for (result <- future) yield {
      assertEquals(result, false)
    }
  }
}
