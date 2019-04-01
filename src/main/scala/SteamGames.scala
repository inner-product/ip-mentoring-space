package notes

object SteamGames {
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
