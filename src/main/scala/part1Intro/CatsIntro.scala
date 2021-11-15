package part1Intro

object CatsIntro {

  // Eq


  // part 1 - type class import

  import cats.Eq


  // part 2 - import type class instances of the types you need
  import cats.instances.int._

  // part 3 - use type class API
  val intEquality: Eq[Int] = Eq[Int]
  val aTypeSafeComparison: Boolean = intEquality.eqv(2, 3)

  // part 4 - use extension methods (if applicable)

  import cats.syntax.eq._

  val anotherTypeSafeComparison: Boolean = 2 === 3
  val neqTypeSafeComparison: Boolean = 2 =!= 3
  // extension methods are only visible in the presence of the right type classes instances

  // part 5 - extending the type class operations to composite types such as lists

  import cats.instances.list._

  val aListComparison: Boolean = List(2) === List(3)

  //  part 6 - create type class instance for a custom type
  case class ToyCar(model: String, price: Double)

  implicit val toyCarEq: Eq[ToyCar] = Eq.instance[ToyCar] { (car1, car2) => car1.price == car2.price }

  val compareTwoToyCars: Boolean = ToyCar("Ferrari", 29.99) === ToyCar("Mercedes", 29.99)

}
