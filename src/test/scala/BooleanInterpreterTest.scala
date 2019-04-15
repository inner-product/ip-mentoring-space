package notes
import minitest._

object BooleanInterpreterTest extends SimpleTestSuite {
  import BooleanInterpreter._
  test("moreThanTenAndNonEmptyName true") {
    assert(moreThanTenAndNonEmptyName(11, "abc"))
  }
  test("moreThanTenAndNonEmptyName false") {
    assert(!moreThanTenAndNonEmptyName(9, ""))
  }
  test("MoreThanTen interpreter true") {
    assert(MoreThanTen(11).evaluate)
  }
  test("NonEmptyName interpreter true") {
    assert(NonEmptyName("abc").evaluate)
  }
  test("MoreThanTen and NonEmptyName interpreter true") {
    assert(MoreThanTen(11).evaluate && NonEmptyName("abc").evaluate)
  }
  test("AND interpreter true") {
    assert(And(MoreThanTen(11), NonEmptyName("abc")).evaluate)
  }
}
