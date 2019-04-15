package notes

object SteamGames {
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
}
