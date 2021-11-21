package part5alien

import cats.Monoid

object ContravariantFunctors {


  trait Format[T] {
    self => // contravariant type class
    def format(value: T): String

    def contramap[A](func: A => T): Format[A] = new Format[A] {
      override def format(value: A): String = self.format(func(value))
    }
  }

  // type class pattern
  def format[A](value: A)(implicit f: Format[A]): String = f.format(value)

  implicit object StringFormat extends Format[String] {
    override def format(value: String): String = "\"" + value + "\""
  }

  implicit object IntFormat extends Format[Int] {
    override def format(value: Int): String = value.toString
  }

  implicit object BooleanFormat extends Format[Boolean] {
    override def format(value: Boolean): String = if (value) "Y" else "N"
  }

  // problem given Format[MyType], can we have a format[Option[MyType]] ?

  import cats.instances.option._

  implicit def getOptionFormat[T](implicit f: Format[T], m: Monoid[T]): Format[Option[T]] =
    f.contramap[Option[T]](_.getOrElse(m.empty))
  // f.contramap[Option[T]](_.get)
  // new Format[Option[T]] {    override def format(value: Option[T]): String = f.format(value.get)  }

  //  def contramap[A, T](func: A => T)(implicit f: Format[T]): Format[A] = new Format[A] {
  //    override def format(value: A): String = f.format(func(value))
  //  }

  /*
      order = reverse from the written order
        - second get
        - first get
        - format of Int
   */

  /*
      MAP applies transformation in sequence
      contramap applies transformation in REVERSE sequence
   */

  import cats.Contravariant
  import cats.Show
  import cats.instances.int._

  val showInts: Show[Int] = Show[Int]
  val showOption: Show[Option[Int]] = Contravariant[Show].contramap(showInts)(_.getOrElse(0))

  // extension method

  import cats.syntax.contravariant._

  val showOptionShorter: Show[Option[Int]] = showInts.contramap(_.getOrElse(0))


  def main(args: Array[String]): Unit = {
    println(format("hello"))
    println(format(55))
    println(format(true))

    println(format(Option(53)))
    println(format(Option(Option(53))))


  }
}
