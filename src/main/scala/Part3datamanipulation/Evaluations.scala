package Part3datamanipulation

object Evaluations {

  import cats.Eval

  val nowEval: Eval[Int] = Eval.now {
    println("Computing now")
    5
  }

  val alwaysEval: Eval[Int] = Eval.always {
    println("Computing again")
    55
  }

  val laterEval: Eval[Int] = Eval.later {
    println("computing once")
    555
  }

  val composedEvaluation: Eval[Int] = nowEval.flatMap(value1 => laterEval.map(value2 => value1 + value2))
  val anotherComposedEvaluation: Eval[Int] = for {
    value1 <- nowEval
    value2 <- laterEval
  } yield value1 + value2

  // exercise
  val ex1: Eval[Int] = for {
    a <- laterEval // 1 - println("computing once")  2- ""
    b <- alwaysEval // 1 - println("Computing again") 2- println("Computing again")
    c <- nowEval // 0 - println("Computing now")
    d <- alwaysEval // 1 - println("Computing again") 2- println("Computing again")
  } yield (a + b + c + d) // 1 - INT                        2- INT


  /*
      println("Computing now")
      println("computing once")
      println("Computing again")
      println("Computing again")
      value
      println("Computing again")
      println("Computing again")
      value
   */

  // "remember"
  val dontRecompute: Eval[Int] = alwaysEval.memoize


  val tutorial: Eval[String] = Eval
    .always {
      println("step 1...");
      "put the guitar on your lap"
    }
    .map { step1 => println("step 2 ..."); s"$step1 then put your left hand on the neck" }
    .memoize // remember up to this point
    .map { step12 => println("step3, more complicated"); s"$step12 then with the right hand strike the strings" }

  // exercise

  def defer[T](eval: => Eval[T]): Eval[T] =
    Eval.later(()).flatMap(_ => eval)

  // exercise
  def reverseList[T](list: List[T]): List[T] =
    if (list.isEmpty) list
    else reverseList(list.tail) :+ list.head

  def reverseEval[T](list: List[T]): Eval[List[T]] =
    if (list.isEmpty) Eval.now(list)
    else reverseEval(list.tail).map(_ :+ list.head)

  def reverseEvalWithDefer[T](list: List[T]): Eval[List[T]] =
    if (list.isEmpty) Eval.now(list)
    else defer(reverseEvalWithDefer(list.tail).map(_ :+ list.head))


  def main(args: Array[String]): Unit = {
    //    println("MAIN")
    //
    //    println(nowEval.value)
    //    println()
    //
    //    println(alwaysEval.value)
    //    println(alwaysEval.value)
    //
    //    println()
    //    println(laterEval.value)
    //    println(laterEval.value)

    //    println(ex1.value)
    //    println(ex1.value)

    //    println(tutorial.value)
    //    println(tutorial.value)

    defer(Eval.now {
      println("now")
      42
    }).value
    println(reverseList(List(1,2,3)))

    // println(reverseEval((1 to 10000).toList).value) // stack overflow
    println(reverseEvalWithDefer((1 to 10000).toList).value) // stack overflow

  }
}
