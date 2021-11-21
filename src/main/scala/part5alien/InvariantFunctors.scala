package part5alien

import cats.Monoid

object InvariantFunctors {

  trait Crypto[A] {
    self => // self type
    def encrypt(value: A): String

    def decrypt(encrypted: String): A

    def imap[B](back: B => A, forth: A => B): Crypto[B] = new Crypto[B] {
      override def encrypt(value: B): String = self.encrypt(back(value))

      override def decrypt(encrypted: String): B = forth(self.decrypt(encrypted))
    }
  }


  def encrypt[A](value: A)(implicit crypto: Crypto[A]): String = crypto.encrypt(value)

  def decrypt[A](representation: String)(implicit crypto: Crypto[A]): A = crypto.decrypt(representation)

  implicit val cesarCypher: Crypto[String] = new Crypto[String] {
    override def encrypt(value: String): String = value.map(c => (c + 2).toChar)

    override def decrypt(encrypted: String): String = encrypted.map(c => (c - 2).toChar)
  }

  /*
      How can we support ints double option[String]
   */

  implicit val doubleCrypto: Crypto[Double] = cesarCypher.imap(_.toString, _.toDouble)

  // exercise
  implicit val optionStringCrypto: Crypto[Option[String]] = cesarCypher.imap(_.getOrElse(""), Option(_))

  // exercise if you have Crypto[T] => crypto[Option[T]] if you have a Monoid[T]  in scope
  implicit def optionCrypto[T](implicit crypto: Crypto[T], monoid: Monoid[T]): Crypto[Option[T]] =
    crypto.imap(_.getOrElse(monoid.empty), Option(_))


  import cats.Invariant
  import cats.Show
  import cats.instances.string._ // Show[String]

  val showString: Show[String] = Show[String]
  val showOptionString: Show[Option[String]] = Invariant[Show].imap(showString)(/*forward*/ Option(_))(/*backwards*/ _.getOrElse(""))

  import cats.syntax.invariant._

  val showOptionString2: Show[Option[String]] = showString.imap(Option(_))(_.getOrElse(""))


  // exercise
  trait MyInvariant[W[_]] {
    def imap[A, B](wa: W[A])(forth: A => B)(back: B => A): W[B]
  }

  trait MyContraVariant[W[_]] extends MyInvariant[W] {
    def contramap[A, B](wa: W[A])(back: B => A): W[B]

    override def imap[A, B](wa: W[A])(forth: A => B)(back: B => A): W[B] =
      contramap(wa)(back)
  }

  trait MyFunctor[W[_]] extends MyInvariant[W] { // "covariant" functor
    def map[A, B](wa: W[A])(forth: A => B): W[B]

    override def imap[A, B](wa: W[A])(forth: A => B)(back: B => A): W[B] =
      map(wa)(forth)
  }


  def main(args: Array[String]): Unit = {
    val encrypted = encrypt("Lets encrypt")
    val decrypted = decrypt[String](encrypted)

    println(encrypted)
    println(decrypted)

    println(encrypt(Math.PI))
    println(decrypt[Double](encrypt(Math.PI)))

    println(encrypt(Option("A")))
    println(decrypt[Option[String]](encrypt(Option("A"))))

    import cats.instances.double._
    println(encrypt(Option(Math.PI)))
    println(decrypt[Option[Double]](encrypt(Option(Math.PI))))
  }
}
