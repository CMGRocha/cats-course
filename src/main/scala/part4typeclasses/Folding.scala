package part4typeclasses

import cats.Eval
import cats.kernel.Monoid

object Folding {

  // exercises
  object ListExercises {
    def map[A, B](list: List[A])(f: A => B): List[B] =
      list.foldLeft(Nil: List[B])((state, a) => f(a) +: state).reverse

    def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] =
      list.foldLeft(Nil: List[B])((state, a) => state.foldRight(f(a))(_ :: _))

    def filter[A](list: List[A])(predicate: A => Boolean): List[A] =
      list.foldLeft(Nil: List[A])((state, a) => if (predicate(a)) a +: state else state).reverse

    def combineAll[A](list: List[A])(implicit monoid: Monoid[A]): A =
      list.foldLeft(monoid.empty)((state, a) => monoid.combine(state, a))
  }

  import cats.Foldable
  import cats.instances.list._
  import cats.instances.option._

  val sumList: Int = Foldable[List].foldLeft(List(1, 2, 3), 0)(_ + _)
  val sumOption: Int = Foldable[Option].foldLeft(Option(2), 30)(_ + _)

  // foldRight is stack-safe regardless of your container
  val sumRight: Eval[Int] = Foldable[List].foldRight(List(1, 2, 3), Eval.now(0)) { (num, eval) => eval.map(_ + num) }


  // convenience

  import cats.instances.int._ // Monoid[Int]

  val anotherSum: Int = Foldable[List].combineAll(List(1, 2, 3)) // implicit Monoid[Int]

  import cats.instances.string._

  val mappedConcat: String = Foldable[List].foldMap(List(1, 2, 3))(_.toString) // implicit Monoid[String]

  // nesting

  import cats.instances.vector._

  val intsNested = List(Vector(1, 2, 3), Vector(4, 5, 6))
  val result: Int = (Foldable[List] compose Foldable[Vector]).combineAll(intsNested)

  // extension methods

  import cats.syntax.foldable._

  val sum3: Int = List(1, 2, 3).combineAll // requires Foldable[List] and Monoid[Int]
  val mappedConcat2: String = List(1, 2, 3).foldMap(_.toString) // requires Foldable[List] and Monoid[String]


  def main(args: Array[String]): Unit = {
    import ListExercises._
    val numbers: List[Int] = (1 to 2).toList
    println(map(numbers)(_ + 1))
    println(flatMap(numbers)(x => List(x, x + 1)))
    println(filter(numbers)(x => x > 0))
    import cats.instances.int._ // Monoid[Int]
    println(combineAll(numbers))


  }
}
