package notes
import minitest._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object ObjectTestSuite extends SimpleTestSuite {
  test("should be") {
    assert(Object.test == None)
  }
  test("should be test") {
    assert(Object.testList == Some(List(1,2,3)))
  }
  test("should be None") {
    assert(Object.testList2 == None)
  }
  testAsync("asynchronous execution") {
    val future = Object.getAllUptimes(Object.hostnames)

    for (result <- future) yield {
      assertEquals(result, List(17,16,14))
    }
  }
  testAsync("swapit") {
    val future = Object.swapit(Object.futureList)

    for (result <- future) yield {
      assertEquals(result, List(1,2,3))
    }
  }
  test("optionList") {
    assertEquals(Object.optionList, Some(List(1,2,3)))
  }
  test("optionList2") {
    assertEquals(Object.optionList2, None)
  }
}