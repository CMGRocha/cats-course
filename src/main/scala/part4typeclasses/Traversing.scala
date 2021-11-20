package part4typeclasses

import cats.{Applicative, Monad}

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Traversing {

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(4))
  val servers: List[String] = List("ci.example.com", "staging.example.com", "prod.example.com")

  def getBandwidth(hostname: String): Future[Int] = Future(hostname.length * 80)

  /*
      we have
        - a list[String]
        - a func String => Future[Int]
      we want Future[List[Int]]
   */

  val allBandwidthsManual: Future[List[Int]] = servers.foldLeft(Future(List.empty[Int])) { (state, hostname) =>
    val bandwidthFuture: Future[Int] = getBandwidth(hostname)
    for {
      acc <- state
      bandwidth <- bandwidthFuture
    } yield acc :+ bandwidth
  }

  val allBandwidthsTransverse: Future[List[Int]] = Future.traverse(servers)(getBandwidth)
  val allBandwidthsSequence: Future[List[Int]] = Future.sequence(servers.map(getBandwidth))

  // exercise

  import cats.syntax.applicative._ // pure
  import cats.syntax.flatMap._ // flatMap
  import cats.syntax.functor._ // map
  import cats.syntax.apply._ // mapN


  def listTransverseWithMonad[F[_] : Monad, A, B](list: List[A])(func: A => F[B]): F[List[B]] = {
    list.foldLeft(List.empty[B].pure[F]) { (state, a) =>
      for {
        acc <- state
        result <- func(a)
      } yield acc :+ result
    }
  }

  def listTransverse[F[_] : Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] = {
    list.foldLeft(List.empty[B].pure[F]) { (wrapperState, a) =>
      val wrapperElement: F[B] = func(a)
      (wrapperState, wrapperElement).mapN(_ :+ _)
    }
  }

  // exercise
  def listSequence[F[_] : Applicative, A](list: List[F[A]]): F[List[A]] =
  // list.foldLeft(List.empty[A].pure[F])((wrapperState, a) => (wrapperState, a).mapN(_ :+ _))
    listTransverse(list)(identity)

  // exercise

  import cats.instances.vector._

  val x: Vector[List[Int]] = listSequence(List(Vector(1, 2), Vector(3, 4))) // Vector(List(1,2,3,4)) WRONG!!! -> vector list 1 3 list 1 4 list 2 3 list 2 4
  val y: Vector[List[Int]] =listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6)))


  def main(args: Array[String]): Unit = {
    println(x)
    println(y)

  }

}
