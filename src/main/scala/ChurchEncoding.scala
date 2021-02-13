package notes

import cats._
import cats.implicits._
import cats.syntax._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object ChurchEncoding {
  //sealed trait Expr[T]
  //case class Lit[T](value: T) extends Expr[T]
  //case class GreaterThan(value1: Int, value2: Int) extends Expr[Boolean]


  sealed trait Expr_DSL[F[_]] {
    def lit[T](value: T):F[T]
    def greaterThan(value1: Int, value2: Int): F[Boolean]
    def lessThan(value1: Int, value2: Int): F[Boolean]
    def and(a: Boolean, b: Boolean): F[Boolean]
  }

/*
  def program[F[_]](expr: Expr_DSL[F])(implicit m: Monad): F[Boolean] = {
    import expr._
    for {
      a <- lit(1)
      b <- lit(3)
      gt <- greaterThan(a, b)
    } yield gt
  }
  */
  def program[F[_]](expr: Expr_DSL[F]): F[Boolean] = {
    import expr._
    greaterThan(1, 3)
  }
  def program2[F[_]: Monad](expr: Expr_DSL[F]): F[Boolean] = {
    import expr._
    for {
      a <- lit(1)
      b <- lit(3)
      gt <- greaterThan(a, b)
    } yield gt
  }
  def program3[F[_]](expr: Expr_DSL[F])(implicit m: Monad[F]): F[Boolean] = {
    import expr._
    for {
      a <- lit(1)
      b <- lit(3)
      gt <- greaterThan(a, b)
    } yield gt
  }
  object AsyncInterpreter extends Expr_DSL[Future] {
    def lit[T](value: T): Future[T] = {
      Future(value)
    }
    def greaterThan(value1: Int, value2: Int): Future[Boolean] = Future(value1 > value2)
    def lessThan(value1: Int, value2: Int): Future[Boolean] = Future(value1 < value2)
    def and(a: Boolean, b: Boolean): Future[Boolean] = Future(a && b)
  }
  object Interpreter extends Expr_DSL[Id] {
    def lit[T](value: T): Id[T] = {
      value
    }
    def greaterThan(value1: Int, value2: Int): Id[Boolean] = value1 > value2
    def lessThan(value1: Int, value2: Int): Id[Boolean] = value1 < value2
    def and(a: Boolean, b: Boolean): Id[Boolean] = a && b
  }
}
