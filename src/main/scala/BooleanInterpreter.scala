package notes

object BooleanInterpreter {
  sealed trait Predicate {
    def evaluate: Boolean = this match {
      case MoreThanTen(n) => n > 10
      case NonEmptyName(s) => s.nonEmpty
      case And(a,b) => a.evaluate && b.evaluate
    }
  }
  final case class MoreThanTen(n: Int) extends Predicate
  final case class NonEmptyName(s: String) extends Predicate
  final case class And(a: Predicate, b: Predicate) extends Predicate

  val moreThanTen: Int => Boolean = _ > 10
  val nonEmptyName: String => Boolean = _.nonEmpty
  val moreThanTenAndNonEmptyName: (Int, String) => Boolean = (a, b) => moreThanTen(a) && nonEmptyName(b)
}
