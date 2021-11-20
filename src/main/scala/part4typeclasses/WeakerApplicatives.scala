package part4typeclasses

import cats.{Functor, Semigroupal}

object WeakerApplicatives {
  //

  trait MyApply[W[_]] extends Functor[W] with Semigroupal[W] {
    override def product[A, B](wa: W[A], wb: W[B]): W[(A, B)] = {
      val functionWrapper: W[B => (A, B)] = map(wa)(a => (b: B) => (a, b))
      ap(functionWrapper)(wb)
    }

    def mapN[A, B, C](tuple: (W[A], W[B]))(f: (A, B) => C): W[C] = {
      val tupleWrapper = product(tuple._1, tuple._2)
      map(tupleWrapper) {
        case (a, b) => f(a, b)
      }
    }

    def ap[B, T](wf: W[B => T])(wa: W[B]): W[T] // fundamental method of Apply
  }

  trait MyApplicative[W[_]] extends MyApply[W] {
    def pure[A](x: A): W[A] //Fundamental
  }

  import cats.Apply
  import cats.instances.option._

  val applyOption: Apply[Option] = Apply[Option]
  val funcApp: Option[Int] = applyOption.ap(Some((x: Int) => x + 1))(Some(2))

  import cats.syntax.apply._

  val tupleOfOptions: (Option[Int], Option[Int], Option[Int]) = (Option(1), Option(2), Option(3))
  val optionOfTuple: Option[(Int, Int, Int)] = tupleOfOptions.tupled
  val sumOption: Option[Int] = tupleOfOptions.mapN(_ + _ + _)


}
