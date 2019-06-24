package notes
import minitest._
import notes.ObjectTestSuite.{assert, test}
import notes.SteamGames.Review
import cats.implicits._
import cats.data._

object SteamGamesTest extends SimpleTestSuite {
  import SteamGames._
  test("create steam game") {
    val spaceShooter = SteamGames.Game("Space Shooter",
      "This is a game where you shoot stuff in space",
      SteamGames.Action(),
      SteamGames.PG(),
      9.99,
      List.empty[Review])
    assert(spaceShooter.title == "Space Shooter")
    assert(spaceShooter.description == "This is a game where you shoot stuff in space")
    assert(spaceShooter.genre == SteamGames.Action())
    assert(spaceShooter.rating == SteamGames.PG())
    assert(spaceShooter.reviews.isEmpty)
  }
  test("steam game has review") {
    val reviewList = List(SteamGames.Review(8,"Great Game!"))
    val spaceShooter = SteamGames.Game("Space Shooter",
      "This is a game where you shoot stuff in space",
      SteamGames.Action(),
      SteamGames.PG(),
      9.99,
      reviewList)
    assert(spaceShooter.title == "Space Shooter")
    assert(spaceShooter.description == "This is a game where you shoot stuff in space")
    assert(spaceShooter.genre == SteamGames.Action())
    assert(spaceShooter.rating == SteamGames.PG())
    assert(spaceShooter.reviews.size == 1)
    assert(spaceShooter.reviews.headOption.isDefined)
  }
  test("steam game has review properties") {
    val reviewList = List(SteamGames.Review(8,"Great Game!"))
    val spaceShooter = SteamGames.Game("Space Shooter",
      "This is a game where you shoot stuff in space",
      SteamGames.Action(),
      SteamGames.PG(),
      9.99,
      reviewList)
    assert(spaceShooter.reviews.head.score == 8)
    assert(spaceShooter.reviews.head.reviewText == "Great Game!")
  }
  test("steam game has correct review properties") {
    val reviewList = List(SteamGames.Review(8,"Great Game!"))
    val spaceShooter = SteamGames.Game("Space Shooter",
      "This is a game where you shoot stuff in space",
      SteamGames.Action(),
      SteamGames.PG(),
      9.99,
      reviewList)
    assert(spaceShooter.reviews.exists(review => review.reviewText == "Great Game!" && review.score == 8))
  }
  test("steam game with no reviews has average score 0.0") {
    val reviewList = List.empty[Review]
    val spaceShooter = SteamGames.Game("Space Shooter",
      "This is a game where you shoot stuff in space",
      SteamGames.Action(),
      SteamGames.PG(),
      9.99,
      reviewList)
    assert(spaceShooter.averageScore == 0.0)
  }
  test("steam game with reviews has average score") {
    val reviewList = List(SteamGames.Review(8,"Great Game!"),SteamGames.Review(5,"Ok Game"))
    val spaceShooter = SteamGames.Game("Space Shooter",
      "This is a game where you shoot stuff in space",
      SteamGames.Action(),
      SteamGames.PG(),
      9.99,
      reviewList)
    assert(spaceShooter.averageScore == 6.5)
  }
  test("steam game with non empty title and price greater than 5") {
    val reviewList = List.empty[Review]
    val game = SteamGames.Game("Space Shooter",
      "This is a game where you shoot stuff in space",
      SteamGames.Action(),
      SteamGames.PG(),
      9.99,
      reviewList)
    assert(And(NonEmptyTitle(game), PriceMoreThan(game, 5)).evaluate)
  }
  test("steam game with price less than 0") {
    val reviewList = List.empty[Review]
    val game = SteamGames.Game("Space Shooter",
      "This is a game",
      SteamGames.Action(),
      SteamGames.PG(),
      -1.0,
      reviewList)
    val validation = Game.validateGame(game)
    assertEquals(validation, cats.data.Validated.invalid(NonEmptyList.one("price is not negative")))
  }
  test("steam game with description length greater than 20") {
    val reviewList = List.empty[Review]
    val game = SteamGames.Game("Space Shooter",
      "This is a game where you shoot stuff in space",
      SteamGames.Action(),
      SteamGames.PG(),
      1,
      reviewList)
    val validation = Game.validateGame(game)
    assertEquals(validation, cats.data.Validated.invalid(NonEmptyList.one("description is less than or equal to 20 characters")))
  }
  test("steam game with both errors") {
    val reviewList = List.empty[Review]
    val game = SteamGames.Game("Space Shooter",
      "This is a game where you shoot stuff in space",
      SteamGames.Action(),
      SteamGames.PG(),
      -1,
      reviewList)
    val validation = Game.validateGame(game)
    assertEquals(validation, cats.data.Validated.invalid(NonEmptyList.one("price is not negative")))
  }
}
