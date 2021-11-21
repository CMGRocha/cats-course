package part5alien

object Kleislis {

  val func1: Int => Option[String] = x => if (x % 2 == 0) Some(s"$x is even") else None
  val func2: Int => Option[Int] = x => Some(x * 3)

  // val func3 = func2 andThen func1 -> can't handle the Option without unwrapping

  val plainFunction1: Int => String = x => if (x % 2 == 0) s"$x is even" else "fail"
  val plainFunction2: Int => Int = x => x * 3
  val plainFunction3: Int => String = plainFunction2 andThen plainFunction1

  import cats.data.Kleisli
  import cats.instances.option._ // FlatMap[Option]

  val func1K: Kleisli[Option, Int, String] = Kleisli(func1)
  val func2K: Kleisli[Option, Int, Int] = Kleisli(func2)
  val func3K: Kleisli[Option, Int, String] = func2K andThen func1K

  // convenience
  val multiply: Kleisli[Option, Int, Int] = func2K.map(_ * 2) // x => Option(...).map(_ * 2)
  val chain: Kleisli[Option, Int, String] = func2K.flatMap(_ => func1K)

  import cats.Id
  import cats.data.Reader

  type InterstingKleislis[A, B] = Kleisli[Id, A, B] // wrapper over A => Id[B]
  // hint
  val times2: Kleisli[Id, Int, Int] = Kleisli[Id, Int, Int](x => x * 2)

  // Reader[A,B] is a Kleisli[Id, A, B]
  val times2Reader: Reader[Int, Int] = Kleisli[Id, Int, Int](x => x * 2)

  val plus4: Kleisli[Id, Int, Int] = Kleisli[Id, Int, Int](x => x + 4)

  val composed: Kleisli[Id, Int, Int] = times2.flatMap(t2 => plus4.map(p4 => t2 + p4))
  val composedFor: Kleisli[Id, Int, Int] = for {
    t2 <- times2
    p4 <- plus4
  } yield t2 + p4


  def main(args: Array[String]): Unit = {
    println(composedFor)
  }
}
