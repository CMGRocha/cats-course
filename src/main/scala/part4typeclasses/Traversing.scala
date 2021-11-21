package part4typeclasses

import cats.{Applicative, Foldable, Functor, Monad, data}

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
  val y: Vector[List[Int]] = listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6)))

  import cats.instances.option._

  def filterAsOption(list: List[Int])(predicate: Int => Boolean): Option[List[Int]] =
    listTransverse(list)(x => Some(x).filter(predicate))

  // exercise
  val allTrue: Option[List[Int]] = filterAsOption(List(2, 4, 6))(_ % 2 == 0) // Some(List(2,4,6))
  val someFalse: Option[List[Int]] = filterAsOption(List(1, 2, 3))(_ % 2 == 0)

  import cats.data.Validated
  import cats.instances.list._ // Semigroup[List] => Applicative[ErrorsOr]

  type ErrorsOr[T] = Validated[List[String], T]

  def filterAsValidated(list: List[Int])(predicate: Int => Boolean): ErrorsOr[List[Int]] =
    listTransverse[ErrorsOr, Int, Int](list) { n =>
      if (predicate(n)) Validated.valid(n)
      else Validated.invalid(List(s"Predicate for $n failed"))
    }

  // exercise
  val allTrueValidated: ErrorsOr[List[Int]] = filterAsValidated(List(2, 4, 6))(_ % 2 == 0)
  val someFalseValidated: ErrorsOr[List[Int]] = filterAsValidated(List(1, 2, 3))(_ % 2 == 0)

  trait MyTraverse[L[_]] extends Foldable[L] with Functor[L]{
    def transverse[F[_] : Applicative, A, B](container: L[A])(func: A => F[B]): F[L[B]]

    def sequence[F[_] : Applicative, A, B](container: L[F[A]]): F[L[A]] =
      transverse(container)(identity)

    type Identity[T] = T

    // exercise
    def mapWithoutCats[A, B](wa: L[A])(f: A => B): L[B] =
    // transverse(wa)(a => f(a): Identity[B])
      transverse[Identity, A, B](wa)(f)

    import cats.Id

    def map[A, B](wa: L[A])(f: A => B): L[B] =
      transverse[Id, A, B](wa)(f)
  }



  /*
  val allBandwidthsManual: Future[List[Int]] = servers.foldLeft(Future(List.empty[Int])) { (state, hostname) =>
  val bandwidthFuture: Future[Int] = getBandwidth(hostname)
  for {
    acc <- state
    bandwidth <- bandwidthFuture
  } yield acc :+ bandwidth
}
 */

  import cats.Traverse
  // import cats.instances.list._ // Semigroup[List]  || already imported
  import cats.instances.future._ // Applicative[Future]

  val allBandwidthCats: Future[List[Int]] = Traverse[List].traverse(servers)(getBandwidth)

  // extension methods

  import cats.syntax.traverse._

  val allBandwidthCats2: Future[List[Int]] = servers.traverse(getBandwidth)


  def main(args: Array[String]): Unit = {
    println(x)
    println(y)
    println(allTrue)
    println(someFalse)

    println(allTrueValidated)
    println(someFalseValidated)

  }

}
