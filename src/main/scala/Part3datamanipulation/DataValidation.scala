package Part3datamanipulation

import cats.Semigroup

import scala.annotation.tailrec
import scala.util.Try

object DataValidation {

  import cats.data.Validated

  val aValidValue: Validated[String, Int] = Validated.valid(55) // "right"
  val invalidValue: Validated[String, Int] = Validated.invalid("something went wrong") // "left"
  val aTest: Validated[String, Int] = Validated.cond(42 > 1, 55, "meaning of life is too small")


  // exercise
  def isPrime(n: Int): Boolean = {
    @tailrec
    def tailRec(divisor: Int): Boolean =
      if (divisor <= 1) true
      else n % divisor != 0 && tailRec(divisor - 1)

    if (n == 0 || n == 1 || n == -1) false
    else tailRec(math.abs(n / 2))
  }

  def testNumber(n: Int): Either[List[String], Int] = {
    val isNotEven: List[String] = if (n % 2 == 0) List() else List("Number must be even")
    val isNegative: List[String] = if (n >= 0) List() else List("Number is negative")
    val isTooBig: List[String] = if (n < 100) List() else List("Number must be less or equal to 100")
    val isNotPrime: List[String] = if (isPrime(n)) List() else List("Number must be prime")

    val resultList = isNotEven ++ isNegative ++ isTooBig ++ isNotPrime
    if (resultList.isEmpty) Right(n)
    else Left(resultList)
  }

  import cats.instances.list._

  implicit val combineIntMax: Semigroup[Int] = Semigroup.instance[Int](math.max)

  def validatedNumber(n: Int): Validated[List[String], Int] =
    Validated.cond(n % 2 == 0, n, List("Number must be even"))
      .combine(Validated.cond(n >= 0, n, List("Number is negative")))
      .combine(Validated.cond(n < 100, n, List("Number must be less or equal to 100")))
      .combine(Validated.cond(isPrime(n), n, List("Number must be prime")))


  // chain
  aValidValue.andThen(_ => invalidValue) // like flatmap but keeps "left" side
  // te st a valid value
  aValidValue.ensure(List("something went wrong"))(_ % 2 == 0)
  // transform
  aValidValue.map(_ + 1)
  aValidValue.leftMap(_ + "!!!")
  aValidValue.bimap(_ + "!!!", _ + 1)
  // interoperate with stdlib
  val eitherToValidated: Validated[List[String], Int] = Validated.fromEither(Right(55))
  val optionToValidated: Validated[List[String], Int] = Validated.fromOption(Option(2), List("If other value is none this is triggered"))
  val tryToValidated: Validated[Throwable, Int] = Validated.fromTry(Try("not an int".toInt))

  // backwards
  aValidValue.toOption
  aValidValue.toEither

  // exercise
  object FormValidation {
    type FormValidation[T] = Validated[List[String], T]

    def validateForm(form: Map[String, String]): FormValidation[String] =
      Validated
        .fromTry(Try(form("name")))
        .leftMap(_ => List("Name not provided"))
        .ensure(List("Name length Invalid"))(_.nonEmpty)
        .combine(
          Validated
            .fromTry(Try(form("email")))
            .leftMap(_ => List("Email not provided"))
            .ensure(List("Invalid email"))(_.contains("@"))
        )
        .combine(
          Validated
            .fromTry(Try(form("password")))
            .leftMap(_ => List("password not provided"))
            .ensure(List("Invalid password length"))(_.length >= 10)
        )
        .map(_ => "success")
  }

  object FormValidationPretty {
    type FormValidation[T] = Validated[List[String], T]

    def getValue(form: Map[String, String], fieldName: String): FormValidation[String] =
      Validated.fromOption(form.get(fieldName), List(s"The field $fieldName must be specified"))

    def nonBlank(value: String, fieldName: String): FormValidation[String] =
      Validated.cond(value.nonEmpty, value, List(s"The field $fieldName must be non empty"))

    def emailProper(email: String): FormValidation[String] =
      Validated.cond(email.contains("@"), email, List(s"The email must be valid"))

    def passwordChecker(password: String): FormValidation[String] =
      Validated.cond(password.length >= 10, password, List(s"The Password must be at least 10 characters"))

    def validateForm(form: Map[String, String]): FormValidation[String] = {
      getValue(form, "name").andThen(name => nonBlank(name, "Name"))
        .combine(getValue(form, "email").andThen(emailProper))
        .combine(getValue(form, "password").andThen(passwordChecker))
        .map(_ => "Success")
    }
  }

  import cats.syntax.validated._

  val aValidMeaningOfLife: Validated[List[String], Int] = 42.valid[List[String]]
  val anInvalidMeaningOfLife: Validated[List[String], Int] = List("something is wrong").invalid[Int]

  def main(args: Array[String]): Unit = {

    val input: Map[String, String] = Map(
      "name" -> "CR",
      "email" -> "@",
      "password" -> "0123456789!"
    )
    println(FormValidation.validateForm(input))
  }
}
