package Part3datamanipulation

import cats.Semigroup

import scala.annotation.tailrec

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


  def main(args: Array[String]): Unit = {

  }
}
