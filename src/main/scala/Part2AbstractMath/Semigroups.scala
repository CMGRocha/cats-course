package Part2AbstractMath

object Semigroups {

  // Semigroups COMBINE elements of the same type

  import cats.Semigroup
  import cats.instances.int._

  val naturalIntSemigroup: Semigroup[Int] = Semigroup[Int]
  val intCombination: Int = naturalIntSemigroup.combine(5, 55) // addition

  import cats.instances.string._

  val naturalStringSemigroup: Semigroup[String] = Semigroup[String]
  val stringCombination: String = naturalStringSemigroup.combine("hello", "world") // concatenation

  // specific API
  def reduceInts(list: List[Int]): Int = list.reduce(naturalIntSemigroup.combine)

  def reduceStrings(list: List[String]): String = list.reduce(naturalStringSemigroup.combine)

  // general API
  def reduceThings[T](list: List[T])(implicit semigroup: Semigroup[T]): T = list.reduce(semigroup.combine)

  // exercise : support a new type
  case class Expense(id: Long, amount: Double)

  implicit val expenseSemigroup: Semigroup[Expense] = Semigroup.instance[Expense]((expense1, expense2) =>
    Expense(id = expense1.id max expense2.id, amount = expense1.amount + expense2.amount))

  // extension methods from semigroup - |+|

  import cats.syntax.semigroup._

  val anIntSum: Int = 2 |+| 3 // requires the presence of an implicit Semigroup[Int]
  val anStringConcat: String = "hello" |+| "world"
  val aCombineExpense: Expense = Expense(3, 5.00) |+| Expense(4, 1.00)

  // exercise : implement reduceThings2 with the |+|
  def reduceThings2[T: Semigroup](list: List[T]): T = list.reduce(_ |+| _)

  def main(args: Array[String]): Unit = {

    // specific API
    val numbers: List[Int] = (1 to 10).toList
    println(reduceInts(numbers))
    val strings: List[String] = List("hello", "world", "from", "lists")
    println(reduceStrings(strings))

    // generic API
    println(reduceThings(numbers))
    println(reduceThings(strings))

    import cats.instances.option._

    val listOfOptions: List[Option[Int]] = numbers.map(Option(_))
    println(reduceThings(listOfOptions))

    val expense1 = Expense(1, 22.99)
    val expense2 = Expense(2, 3.01)
    val expense3 = Expense(3, 5.00)
    val expenses = List(expense1, expense2, expense3)
    println(reduceThings(expenses))
    println(reduceThings2(expenses))


  }
}
