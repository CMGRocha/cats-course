package Part2AbstractMath

import scala.annotation.tailrec

object CustomMonads {

  import cats.Monad

  implicit object OptionMonad extends Monad[Option] {
    override def pure[A](x: A): Option[A] = Option(x)

    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)


    // hast to be stack safe!
    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] = f(a) match {
      case None => None
      case Some(Right(value)) => Some(value)
      case Some(Left(value)) => tailRecM(value)(f)
    }
  }

  type Identity[T] = T
  val aNumber: Identity[Int] = 44

  implicit object IdentityMonad extends Monad[Identity] {
    override def pure[A](x: A): Identity[A] = x

    override def flatMap[A, B](a: Identity[A])(f: A => Identity[B]): Identity[B] = f(a)

    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Identity[Either[A, B]]): Identity[B] = f(a) match {
      case Right(value) => value
      case Left(value) => tailRecM(value)(f)
    }
  }

  sealed trait Tree[+A]

  final case class Leaf[+A](value: A) extends Tree[A]

  final case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {
    // smart constructor
    def leaf[T](value: T): Tree[T] = Leaf(value)

    def branch[T](left: Tree[T], right: Tree[T]): Tree[T] = Branch(left, right)
  }


  implicit object TreeMonad extends Monad[Tree] {
    override def pure[A](x: A): Tree[A] = Tree.leaf(x)

    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
      case Leaf(value) => f(value)
      case Branch(left, right) => Branch(flatMap(left)(f), flatMap(right)(f))
    }

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
      def stackRecursion(t: Tree[Either[A, B]]): Tree[B] = t match {
        case Leaf(Right(value)) => Tree.leaf(value)
        case Leaf(Left(value)) => stackRecursion(f(value))
        case Branch(left, right) => Branch(stackRecursion(left), stackRecursion(right))
      }

      // stackRecursion(f(a))
      @tailrec
      def tailRec(todo: List[Tree[Either[A, B]]], expanded: List[Tree[Either[A, B]]] = Nil, done: List[Tree[B]] = Nil): Tree[B] =
        if (todo.isEmpty) done.head
        else todo.head match {
          case Leaf(Left(value)) => tailRec(f(value) :: todo.tail, expanded, done)
          case Leaf(Right(value)) => tailRec(todo.tail, expanded, Leaf(value) :: done)
          case node@Branch(left, right) =>
            if (!expanded.contains(node)) {
              tailRec(left :: right :: todo, expanded :+ node, done)
            } else {
              val newLeft = done.head
              val newRight = done.tail.head
              val newBranch = Branch(newLeft, newRight)
              tailRec(todo.tail, expanded, newBranch :: done.drop(2))
            }
        }

      tailRec(List(f(a)))
    }
  }

  def main(args: Array[String]): Unit = {
    val tree: Tree[Int] = Branch(Leaf(10), Leaf(20))
    val changedTree = TreeMonad.flatMap(tree)(v => Branch(Leaf(v + 1), Leaf(v + 2)))
    println(changedTree)
  }
}