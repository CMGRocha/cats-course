package part4typeclasses

object Applicatives {

  // Applicatives = Functors + the pure method

  import cats.Applicative
  import cats.instances.list._

  val listApplicative: Applicative[List] = Applicative[List]
  val aList: List[Int] = listApplicative.pure(2)

  import cats.instances.option._

  val optionApplicative: Applicative[Option] = Applicative[Option]
  val anOption: Option[Int] = optionApplicative.pure(7)

  // pure extension method

  import cats.syntax.applicative._

  val aSweetList: List[Int] = 2.pure[List]
  val aSweetOption: Option[Int] = 2.pure[Option]


  // Monads extend Applicative
  // Applicatives extend Functors

  import cats.data.Validated

  type ErrorOr[T] = Validated[List[String], T]

  val aValidValue: ErrorOr[Int] = Validated.valid(45) // pure method
  val aModifiedValidated: ErrorOr[Int] = aValidValue.map(_ + 1) // map function
  val validatedApplicative: Applicative[ErrorOr] = Applicative[ErrorOr]

  // exercise
  def ap[W[_], B, TUPLE](wf: W[B => TUPLE])(wa: W[B]): W[TUPLE] = ???

  def productWithApplicatives[W[_], A, B](wa: W[A], wb: W[B])(implicit applicative: Applicative[W]): W[(A, B)] = {
    val functionWrapper: W[B => (A, B)] = applicative.map(wa)(a => (b: B) => (a, b))
    applicative.ap(functionWrapper)(wb)
  }

  // applicatives have this function ap[W[_], B, TUPLE](wf: W[B => TUPLE])(wa: W[B]): W[TUPLE]
  // applicatives can implement product from semigroupal
  // => applicatives extend semigroupal


  // so far
  // Monads extend Applicative
  // Applicatives extend Functors
  // Applicative extend semigroupal


}
