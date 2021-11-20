package part4typeclasses

import cats.{Applicative, Monad}

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object HandingErrors {

  trait MyApplicativeError[M[_], E] extends Applicative[M] {
    // pure from applicative
    def raiseError[A](e: E): M[A] // fundamental method

    def handleErrorWith[A](ma: M[A])(func: E => M[A]): M[A] // fundamental method

    def handleError[A](ma: M[A])(func: E => A): M[A] = handleErrorWith(ma)(e => pure(func(e)))
  }

  trait MyMonadError[M[_], E] extends MyApplicativeError[M, E] with Monad[M] {
    def ensure[A](ma: M[A])(error: E)(predicate: A => Boolean): M[A] // fundamental method
  }

  import cats.MonadError
  import cats.instances.either._

  type ErrorOr[A] = Either[String, A]
  val monadErrorEither: MonadError[ErrorOr, String] = MonadError[ErrorOr, String]
  val success: ErrorOr[Int] = monadErrorEither.pure(32)
  val failure: ErrorOr[Int] = monadErrorEither.raiseError[Int]("Something wrong")

  // "recover
  val handledError: ErrorOr[Int] = monadErrorEither.handleError(failure) {
    case "something" => 44 // convert MonadError[ErrorOr, String] => MonadError[ErrorOr, Int]
    case _ => 89
  }

  // "recoverWith
  val handledError2: ErrorOr[Int] = monadErrorEither.handleErrorWith(failure) {
    case "something" => monadErrorEither.pure(44) // ErrorOr[Int]
    case _ => Left("Something else") //ErrorOr[Int]
  }

  // "filter"
  val filteredSuccess: ErrorOr[Int] = monadErrorEither.ensure(success)("number too small")(_ > 100)

  // try and future

  import cats.instances.try_._ // implicit MonadError[Try], E = Throwable

  val exception = new RuntimeException("Really bad")
  val pureException: Try[Int] = MonadError[Try, Throwable].raiseError(exception) // Failure(exception)

  import cats.instances.future._

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(4))
  MonadError[Future, Throwable].raiseError(exception) // Future which will complete with a Failure(exception)

  // applicatives => ApplicativeError

  import cats.data.Validated
  import cats.instances.list._
  import cats.ApplicativeError

  type ErrorsOr[T] = Validated[List[String], T]
  val applicativeErrorValidated: ApplicativeError[ErrorsOr, List[String]] = ApplicativeError[ErrorsOr, List[String]]
  // has access to pure, raise error, handleError, handleErrorWith

  // Extension methods

  import cats.syntax.applicative._ // pure
  import cats.syntax.applicativeError._ // raise error, handleError, handleErrorWith

  val extendedSuccess: ErrorOr[Int] = 42.pure[ErrorOr] // requires implicit ApplicativeError[ErrorsOr, List[String]]
  val extendedError: ErrorsOr[Int] = List("boom").raiseError[ErrorsOr, Int]

  val recoveredError: ErrorsOr[Int] = extendedError.recover {
    case _ => 43
  }

  import cats.syntax.monadError._

  val testedSuccess: ErrorOr[Int] = success.ensure("Something bad")(_ > 100)
}
