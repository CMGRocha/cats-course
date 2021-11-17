package Part2AbstractMath

object Monoids {

  import cats.Semigroup
  import cats.instances.int._
  import cats.syntax.semigroup._

  val numbers: List[Int] = (1 to 1000).toList
  // |+| is always associative
  val sumLeft: Int = numbers.foldLeft(0)(_ |+| _)
  val sumRight: Int = numbers.foldRight(0)(_ |+| _)

  // define a general API
  //  def combineFold[T](list: List[T])(implicit semigroup: Semigroup[T]): T =
  //    list.foldLeft(/* NO VALID INITIAL VALUE */)(_ |+| _)


  // MONOID - same as semigroup with the "zero" value

  import cats.Monoid

  val intMonoid: Monoid[Int] = Monoid[Int]
  val combineInt: Int = intMonoid.combine(1, 2)
  val zero: Int = intMonoid.empty

  import cats.instances.string._ // Monoid[String]

  val emptyString: String = Monoid[String].empty
  val combineStrings: String = Monoid[String].combine("hello", "world")

  import cats.instances.option._ // Monoid[Option[Int]]

  val emptyOption: Option[Int] = Monoid[Option[Int]].empty
  val combineOption: Option[Int] = Monoid[Option[Int]].combine(Option(2), Option.empty[Int])
  val combineOption2: Option[Int] = Monoid[Option[Int]].combine(Option(2), Option(3))

  // extension methods for Monoids -  |+|

  // import cats.syntax.monoid._ // or import cats.syntax.semigroup._

  val combineOptionFancy: Option[Int] = Option(2) |+| Option(2)

  // exercise
  def combineFold[T](list: List[T])(implicit monoid: Monoid[T]): T =
    list.foldLeft(monoid.empty)(_ |+| _)

  // exercise
  val phonebooks = List(
    Map(
      "Alice" -> 235,
      "Bob" -> 647
    ),
    Map(
      "Charlie" -> 372,
      "Daniel" -> 889
    ),
    Map(
      "Tina" -> 555
    )
  )

  import cats.instances.map._

  val singlePhoneList: Map[String, Int] = combineFold(phonebooks)

  // exercise

  case class ShoppingCart(items: List[String], total: Double)

  implicit val shoppingCartMonoid: Monoid[ShoppingCart] =
    Monoid.instance[ShoppingCart](ShoppingCart(Nil, 0.0), (x, y) => ShoppingCart(items = x.items ++ y.items, total = x.total + y.total))

  def checkout(shoppingCarts: List[ShoppingCart]): ShoppingCart =
    combineFold(shoppingCarts)

  def main(args: Array[String]): Unit = {
    println(sumLeft)
    println(sumRight)

    println(combineFold(numbers))
    println(singlePhoneList)
  }
}
