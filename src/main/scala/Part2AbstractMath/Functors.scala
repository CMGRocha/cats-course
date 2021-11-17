package Part2AbstractMath

import scala.util.Try

object Functors {

  val aModifiedList: List[Int] = List(1, 2, 3).map(_ + 1)
  val aModifiedOption: Option[Int] = Option(2).map(_ + 1)
  val aModifiedTry: Try[Int] = Try(1).map(_ + 1)

  // simplified functor
  trait MyFunctor[F[_]] { // Higher Kinded type
    def map[A, B](initialValue: F[A])(f: A => B): F[B]
  }

  import cats.Functor
  import cats.instances.list._ // includes Functor[List]

  val listFunctor: Functor[List] = Functor[List] // Higher kinded type
  val incrementedNumbers: List[Int] = listFunctor.map(List(1, 2, 3))(_ + 1)

  import cats.instances.option._ // includes Functor[Option]

  val optionFunctor: Functor[Option] = Functor[Option]
  val incrementedOption: Option[Int] = optionFunctor.map(Option(1))(_ + 1)

  import cats.instances.try_._ // includes Functor[Try]

  val tryFunctor: Functor[Try] = Functor[Try]
  val incrementedTry: Try[Int] = tryFunctor.map(Try(1))(_ + 1)

  // generalizing an API
  def do10xList(list: List[Int]): List[Int] = list.map(_ * 10)

  def do10xOption(option: Option[Int]): Option[Int] = option.map(_ * 10)

  def do10xTry(attempt: Try[Int]): Try[Int] = attempt.map(_ * 10)

  // generalize
  def do10x[F[_]](container: F[Int])(implicit functor: Functor[F]): F[Int] =
    functor.map(container)(_ * 10)

  // Exercise
  trait Tree[+T]

  object Tree {
    // smart constructor
    def leaf[T](value: T): Tree[T] = Leaf(value)

    def branch[T](value: T, left: Tree[T], right: Tree[T]): Tree[T] = Branch(value, left, right)
  }

  case class Leaf[+T](value: T) extends Tree[T]

  case class Branch[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]

  implicit object treeFunctor extends Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = {
      fa match {
        case Leaf(value) => Leaf(f(value))
        case Branch(value, left, right) =>
          Branch(value = f(value), left = map(left)(f), right = map(right)(f))
      }
    }
  }

  // extension method - map

  import cats.syntax.functor._

  val tree: Tree[Int] = Tree.branch(1, Tree.leaf(2), Tree.leaf(3))
  val incrementedTree: Tree[Int] = tree.map(_ + 1)

  // exercise
  def do10xShorter[F[_] : Functor](container: F[Int]): F[Int] = container.map(_ + 1)

  def main(args: Array[String]): Unit = {
    println(do10x(List(1, 2, 3)))
    println(do10x(Option(2)))
    println(do10x(Try(5)))

    // println(do10x[Tree](Branch(1, Leaf(2), Leaf(3))))
    println(do10x(Tree.branch(1, Tree.leaf(2), Tree.leaf(3))))
  }
}
