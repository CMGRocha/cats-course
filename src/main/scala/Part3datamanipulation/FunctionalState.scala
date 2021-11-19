package Part3datamanipulation

import cats.Eval
import cats.data.IndexedStateT

object FunctionalState {

  type MyState[S, A] = S => (S, A)

  import cats.data.State

  val countAndSay: State[Int, String] = State(currentCount => (currentCount + 1, s" counted $currentCount"))
  val (eleven, counted10) = countAndSay.run(10).value
  // state = "iterative" computations

  // iterative "java" style
  var a = 10
  a += 1
  val firstComputation = s"Added 1 to 10, obtained $a"
  a *= 5
  val secondComputation = s"Multiplied with 5, obtained $a"

  // pure FP with states
  val firstTransformation: State[Int, String] = State((s: Int) => (s + 1, s"Added 1 to $s, obtained ${s + 1}"))
  val secondTransformation: State[Int, String] = State((s: Int) => (s * 5, s"Multiplied by 5, obtained ${s * 5}"))

  val compositeTransformation: State[Int, (String, String)] = firstTransformation.flatMap { firstResult =>
    secondTransformation.map(secondResult => (firstResult, secondResult))
  }
  val compositeTransformation2 = for {
    firstResult <- firstTransformation
    secondResult <- secondTransformation
  } yield (firstResult, secondResult)

  // function composition is clunky
  val func1: Int => (Int, String) = (s: Int) => (s + 1, s"Added 1 to $s, obtained ${s + 1}")
  val func2: Int => (Int, String) = (s: Int) => (s * 5, s"Multiplied by 5, obtained ${s * 5}")
  val compositeFunc: Int => (String, (Int, String)) = // INT is the desired value !! hard to get
    func1.andThen {
      case (newState, firstResult) => (firstResult, func2(newState))
    }

  // exercise
  case class ShoppingCart(items: List[String], total: Double)

  def addToCart(item: String, price: Double): State[ShoppingCart, Double] = {
    State((cart: ShoppingCart) => (ShoppingCart(item :: cart.items, cart.total + price), cart.total + price))
  }

  val carlosCart: State[ShoppingCart, Double] = for {
    _ <- addToCart("Fender guitar", 500)
    _ <- addToCart("Elixir String", 19)
    total <- addToCart("Electric cable", 8)
  } yield total


  // exercise
  // returns a state data structure that, when run, will not change the state but will issue the value f(a)
  def inspect[A, B](f: A => B): State[A, B] =
    State((a: A) => (a, f(a)))

  // returns a state data structure that, when, run,  return the value of that state and makes no change
  def get[A]: State[A, A] =
    State((a: A) => (a, a))

  // returns a state data structure that, when run, returns Unit and sets the state to that value
  def set[A](value: A): State[A, Unit] =
    State(_ => (value, ()))

  // returns a state data structure that, when run, returns unit and sets the state to f(state)
  def modify[A](f: A => A): State[A, Unit] =
    State(a => (f(a), ()))


  val program: State[Int, (Int, Int, Int)] = for {
    a <- get[Int]
    _ <- set[Int](a + 10)
    b <- get[Int]
    _ <- modify[Int](_ + 43)
    c <- inspect[Int, Int](_ * 2)
  } yield (a, b, c)

  def main(args: Array[String]): Unit = {
    println(compositeTransformation.run(10).value)
    println(carlosCart.run(ShoppingCart(List(), 0)).value)

    println(program.run(2).value)
  }
}
