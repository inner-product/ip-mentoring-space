package notes

import cats._
import cats.implicits._
import cats.data._
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object Object {
//def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B
  //def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B]
  //def traverse[G[_], A, B](fa: F[A])(f: (A) => F[B])(implicit arg0: Applicative[G]): G[F[B]]
  //def ap[A, B](ff: F[(A) => B])(fa: F[A]): F[B]

  val someF: Option[Int => Long] = Some(_.toLong + 1L)
  val noneF: Option[Int => Long] = None
  val someInt: Option[Int] = Some(3)
  val noneInt: Option[Int] = None

  //def pure[A](x: A): F[A]

  Apply[Option].ap(someF)(noneInt)

  def test = Apply[Option].ap(someF)(noneInt)
  def test2 = Applicative[Option].pure(1)

  type Hostname = String
  val hostnames: List[Hostname] = List(
    "alpha.example.com",
    "beta.example.com",
    "gamma.demo.com"
  )
  val z = List.empty[Int]
  def getUptime(name: Hostname): Future[Int] = Future(name.length)

  // LinkedList[A]
  // case class EmptyList extends LinkedList
  // case class NonEmptyList(element:A, tail:LinkedList[A]) extends LinkedList
  // val emptyList:LinkedList[Int] = EmptyList()
  // val nonEmptyList:LinkedList[Int] = NonEmptyList(1, emptyList)



  def getAllUptimes(hostnames: List[Hostname]): Future[List[Int]] = {
    hostnames match {
      case Nil => Future(List.empty[Int])
      case h::tail =>
        for {
          x <- getUptime(h)
          ys <- getAllUptimes(tail)
        } yield x :: ys

       // getUptime(h).flatMap{x => x::getAllUptimes(tail).flatMap{y=>y}}
    }
  }

  def parseInt(s: String): Option[Int] = Either.catchOnly[NumberFormatException](s.toInt).toOption
 def myIdentity[A](a: A): A = a
  def testList = List("1", "2", "3").traverse(parseInt)
  def testList2 = List("1", "2", "CDE").traverse(parseInt)
  // def testList3 = List("1", "2", "3").sequence
  def futureList = List(Future(1), Future(2), Future(3))

  //def parseFutureInt(i: Future[Int]): Int = i.
  def swapit(fs: List[Future[Int]]): Future[List[Int]] = {
    fs match {
      case Nil => Future(List.empty[Int])
      case h::tail =>
        for {
          x <- h
          ys <- swapit(tail)
        } yield x :: ys
    }
  }

  def optionList = List(1.some, Option(2), Option(3)).sequence
  def optionList2 = List(4.some, None, None, 5.some).sequence

  val xx = Some(1)
  val yy = 2.some


  //def convert()


  /*
  def getAllUptimes2(hostnames: List[Hostname]): Future[List[Int]] = {
    hostnames foldLeft(Future(List.empty[Int])){
      (acc:Future[List[Int]],hostname:Hostname) => for {
          x <- getUptime(hostname)
          ys <- acc
      } yield x::ys
    }
  }
  */

  def f(x: Int) = List(x.toString)

  val intList: List[Int] = List(1,2,3)
  val stringList = intList flatMap { f }

  getAllUptimes(hostnames)

  //def ap[A, B](ff: F[(A) => B])(fa: F[A]): F[B]



}
