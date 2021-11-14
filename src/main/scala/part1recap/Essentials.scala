package part1recap

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object Essentials {

  // instructions returns Unit -> Action -> Side effects

  // inheritance model : extend at most 1 class but can inherit from 0 or more traits

  val three: Int = 1 + 2
  val anotherThree: Int = 1.+(2)

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(4))

  val aFuture: Future[Int] = Future {
    42
  }
  aFuture.onComplete { //evaluated asynchronously
    case Success(value) => println(s"The async meaning of life is $value")
    case Failure(exception) => println(s"Meaning of life failed with $exception")
  }

  trait HigherKindedType[F[_]]

  trait SequenceChecker[F[_]] {
    def isSequential: Boolean
  }

  val listChecker: SequenceChecker[List] = new SequenceChecker[List] {
    override def isSequential: Boolean = true
  }

  def main(args: Array[String]): Unit = {

  }

}
