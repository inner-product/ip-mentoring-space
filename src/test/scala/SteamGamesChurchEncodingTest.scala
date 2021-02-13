package notes
import minitest._
import notes.ObjectTestSuite.{assertEquals, testAsync}
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object SteamGamesChurchEncodingTest extends SimpleTestSuite {
  import SteamGamesChurchEncoding._
  test("Interpreter true") {
    assert(program(Interpreter))
  }
}
