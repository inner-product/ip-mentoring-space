package notes

import cats.Id
import notes.ChurchEncoding.Expr_DSL

object SteamGamesChurchEncoding {
  /*
  sealed trait Predicate {
    def evaluate: Boolean = this match {
      case AverageScoreMoreThan(g,d) => g.averageScore > d
      case PriceMoreThan(g,d) => g.price > d
      case NonEmptyTitle(g) => g.title.nonEmpty
      case GenreIn(g, genres) => genres.contains(g.genre)
      case RatingIn(g, ratings) => ratings.contains(g.rating)
      case And(a,b) => a.evaluate && b.evaluate
      case Or(a,b) => a.evaluate || b.evaluate
    }
  }
  final case class AverageScoreMoreThan(g: Game, d: Double) extends Predicate
  final case class PriceMoreThan(g: Game, d: Double) extends Predicate
  final case class NonEmptyTitle(g: Game) extends Predicate
  final case class GenreIn(g: Game, genres: List[Genre]) extends Predicate
  final case class RatingIn(g: Game, ratings: List[Rating]) extends Predicate
  final case class And(a: Predicate, b: Predicate) extends Predicate
  final case class Or(a: Predicate, b: Predicate) extends Predicate
  */
  sealed trait Predicate[F[_]] {
    def averageScoreMoreThan(g: Game, d: Double): F[Boolean]
    def priceMoreThan(g: Game, d: Double): F[Boolean]
    def and(a: F[Boolean], b: F[Boolean]): F[Boolean]
  }
  sealed trait PredicateString[F[_]] {
    def averageScoreMoreThan(g: Game, d: Double): F[String]
    def priceMoreThan(g: Game, d: Double): F[String]
    def and(a: F[String], b: F[String]): F[String]
  }
  object Interpreter extends Predicate[Id] {
    def averageScoreMoreThan(g: Game, d: Double): Id[Boolean] = g.averageScore > d
    def priceMoreThan(g: Game, d: Double): Id[Boolean] = g.price > d
    def and(a: Id[Boolean], b: Id[Boolean]): Id[Boolean] = a && b
  }
  object InterpreterString extends PredicateString[Id] {
    def averageScoreMoreThan(g: Game, d: Double): Id[String] = s"${g.averageScore} > $d"
    def priceMoreThan(g: Game, d: Double): Id[String] = s"${g.price} > $d"
    def and(a: Id[String], b: Id[String]): Id[String] = s"${a} AND ${b}"
  }
  final case class Review(score: Int, reviewText: String)

  sealed trait SearchableKey
  sealed trait Genre extends SearchableKey
  final case class Action() extends Genre
  final case class Adventure() extends Genre
  final case class Rpg() extends Genre
  sealed trait Rating extends SearchableKey
  final case class PG() extends Rating
  final case class M() extends Rating
  final case class G() extends Rating

  final case class Game(title: String, description: String, genre: Genre, rating: Rating, price: Double, reviews: List[Review]) {
    def averageScore:Double = {
      def computeTotalScore(reviews: List[Review]): Double = {
        reviews match {
          case Nil => 0.0
          case h :: tail =>
            h.score + computeTotalScore(tail)
        }
      }
      reviews match {
        case Nil => 0.0
        case _ =>
          computeTotalScore(reviews) / reviews.length
      }
    }
  }

  final case class SearchGameCriteria(keyword: Option[String] = None,
                                      genre: List[Genre] = List.empty[Genre],
                                      rating: List[Rating] = List.empty[Rating],
                                      lowPrice: Double = 0.0, highPrice: Double = 0.0,
                                      lowAverageScore: Double = 0.0, highAverageScore: Double = 0.0)
  def program[F[_]](expr: Predicate[F]): F[Boolean] = {
    import expr._
    val reviewList = List(Review(8,"Great Game!"),Review(5,"Ok Game"))
    val spaceShooter = Game("Space Shooter",
      "This is a game where you shoot stuff in space",
      Action(),
      PG(),
      9.99,
      reviewList)
    and(averageScoreMoreThan(spaceShooter, 5), priceMoreThan(spaceShooter, 9))
  }
}
