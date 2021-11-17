package Part2AbstractMath

import cats.Monoid

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Monads {

  // lists
  val numberList = List(1, 2, 3)
  val charsList = List('a', 'b', 'c')

  // exercise - combinations (number char)
  val combinationsList: List[(Int, Char)] = numberList.flatMap(n => charsList.map(c => (n, c)))
  val combinationsListFor: List[(Int, Char)] = for {
    number <- numberList
    char <- charsList
  } yield (number, char)

  // options
  val numberOption: Option[Int] = Option(2)
  val charOption: Option[Char] = Option('d')
  // exercise - combinations (number char)
  val combinationsOption: Option[(Int, Char)] = numberOption.flatMap(n => charOption.map(c => (n, c)))
  val combinationsOptionFor: Option[(Int, Char)] = for {
    number <- numberOption
    char <- charOption
  } yield (number, char)

  //futures
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(4))
  val numberFuture: Future[Int] = Future(42)
  val charFuture: Future[Char] = Future('x')
  val combinationsFuture: Future[(Int, Char)] = numberFuture.flatMap(n => charFuture.map(c => (n, c)))
  val combinationsFutureFor: Future[(Int, Char)] = for {
    number <- numberFuture
    char <- charFuture
  } yield (number, char)

  /*
      Pattern
      - wrapping a value into a monadic(M) value
      - the flatMap mechanism  ( transform)

      MONADS
   */

  trait MyMonad[M[_]] { // Higher kinded type
    def pure[A](value: A): M[A] //

    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

    // exercise
    def map[A, B](ma: M[A])(f: A => B): M[B] =
      flatMap(ma)(a => pure(f(a)))
  }

  import cats.Monad
  import cats.instances.option._ // implicit Monad[Option]

  val optionMonad: Monad[Option] = Monad[Option]
  val anOption: Option[Int] = optionMonad.pure(4) // Option(4) == Some(4)
  val aTransformedOption: Option[Int] = optionMonad.flatMap(anOption)(x => if (x % 3 == 0) Some(x + 1) else None)

  import cats.instances.list._ // implicit Monad[List]

  val listMonad: Monad[List] = Monad[List]
  val aList: List[Int] = listMonad.pure(5)
  val aTransformedList: List[Int] = listMonad.flatMap(aList)(x => List(x, x + 1))

  // Exercise : use a Monad[Future]

  import cats.instances.future._

  val futureMonad: Monad[Future] = Monad[Future]
  val aFuture: Future[Int] = futureMonad.pure(55)
  val aTransformedFuture: Future[Int] = futureMonad.flatMap(aFuture)(x => Future(x + 10000))

  // specialized API
  def getPairsList(numbers: List[Int], chars: List[Char]): List[(Int, Char)] = numbers.flatMap(n => chars.map(c => (n, c)))

  def getPairsOption(number: Option[Int], char: Option[Char]): Option[(Int, Char)] = number.flatMap(n => char.map(c => (n, c)))

  def getPairsFuture(number: Future[Int], char: Future[Char]): Future[(Int, Char)] = number.flatMap(n => char.map(c => (n, c)))

  // Generalized API
  def getPair[M[_], A, B](ma: M[A], mb: M[B])(implicit monad: Monad[M]): M[(A, B)] =
    monad.flatMap(ma)(a => monad.map(mb)(b => (a, b)))

  // extension methods - pure, flatMap

  import cats.syntax.applicative._ // pure is here

  val oneOption: Option[Int] = 1.pure[Option]
  val oneList: List[Int] = 1.pure[List]

  import cats.syntax.flatMap._ // flatMap is here

  val oneOptionTransformed: Option[Int] = oneOption.flatMap(x => (x + 1).pure[Option])

  // Monads extend Functors
  val oneOptionMapped: Option[Int] = Monad[Option].map(Option(2))(_ + 1)

  import cats.syntax.functor._

  val oneOptionMapped2: Option[Int] = oneOption.map(_ + 2)

  // for-comprehension
  val composedOptionFor: Option[Int] = for {
    one <- 1.pure[Option]
    two <- 2.pure[Option]
  } yield (one + two)

  // exercise
  def getPairShort[M[_] : Monad, A, B](ma: M[A], mb: M[B]): M[(A, B)] =
    for {
      a <- ma
      b <- mb
    } yield (a, b)

  def main(args: Array[String]): Unit = {
    println(getPairShort(numberList, charsList))
    println(getPairShort(numberOption, charOption))
    getPairShort(numberFuture, charFuture).foreach(println)
  }

}
