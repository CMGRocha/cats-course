package part4typeclasses

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Semigroupals {

  trait MySemigroupal2[F[_]] {
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }

  import cats.Semigroupal
  import cats.instances.option._

  val optionSemigroupal: Semigroupal[Option] = Semigroupal[Option]
  val aTupledOption: Option[(Int, String)] = optionSemigroupal.product(Some(1123), Some("qwerty"))
  val aNoneTuple = optionSemigroupal.product(Some(12345), None)

  import cats.instances.future._

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(4))
  val aTupledFuture: Future[(Int, String)] = Semigroupal[Future].product(Future(55), Future("a future computation"))

  import cats.instances.list._

  val aTupledList: List[(Int, String)] = Semigroupal[List].product(List(1, 2), List("a", "b"))

  //  Exercise

  import cats.Monad

  import cats.syntax.functor._ // map
  import cats.syntax.flatMap._ //flatmap

  def productWithMonad[F[_], A, B](fa: F[A], fb: F[B])(implicit monad: Monad[F]): F[(A, B)] =
  // monad.flatMap(fa)(a => monad.flatMap(fb)(b => monad.pure((a, b)))) wrong ???
    monad.flatMap(fa)(a => monad.map(fb)(b => (a, b)))

  def productWithMonad2[F[_] : Monad, A, B](fa: F[A], fb: F[B]): F[(A, B)] = for {
    a <- fa
    b <- fb
  } yield (a, b)

  trait MySemigroupal[F[_]] {
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }

  trait MyMonad[M[_]] extends MySemigroupal[M] { // Higher kinded type
    def pure[A](value: A): M[A] //

    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

    // exercise
    def map[A, B](ma: M[A])(f: A => B): M[B] =
      flatMap(ma)(a => pure(f(a)))

    override def product[A, B](fa: M[A], fb: M[B]): M[(A, B)] =
      flatMap(fa)(a => map(fb)(b => (a, b)))
  }


  // Monads extend semigroup.

  // example where semigroup is useful

  import cats.data.Validated

  type ErrorsOr[T] = Validated[List[String], T]
  val validatedSemigroupal: Semigroupal[ErrorsOr] = Semigroupal[ErrorsOr] // requires implicit Semigroup[List[_]]
  val invalidCombinations = validatedSemigroupal.product(
    Validated.invalid(List("Something wrong", "something else went wrong")),
    Validated.invalid(List("this can't be right"))
  )

  type EitherErrorsOr[T] = Either[List[String], T]

  import cats.instances.either._ // implicit Monad[Either]

  val eitherSemigroupal = Semigroupal[EitherErrorsOr]
  val eitherCombination = eitherSemigroupal.product( // in terms of map/flatmap because is a monad
    Left(List("Something wrong", "something else went wrong")),
    Left(List("this can't be right"))
  )

  // exercise
  val zipListSemigroupal: Semigroupal[List] = new Semigroupal[List] {
    override def product[A, B](fa: List[A], fb: List[B]): List[(A, B)] = fa.zip(fb)
  }

  def main(args: Array[String]): Unit = {
    println(aTupledList)
    println(List(1, 2).zip(List("a", "b")))

    println("----")
    println(invalidCombinations)
    println(eitherCombination) // second left is missing
  }
}
