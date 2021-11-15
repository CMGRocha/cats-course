package part1Intro

object TCVariance {

  import cats.eq
  import cats.instances.int._ // EQ[Int] Type class instance
  import cats.instances.option._ // construct Eq[Optional[Int]] type class instance
  import cats.syntax.eq._

  val aComparison: Boolean = Option(2) === Option(3)
  // val aInvalidComparison = Some(2) === None // Eq[Some[Int]] not found
  // val aInvalidComparison2: Boolean = Some(2) === Some(3)

  // variance

}
