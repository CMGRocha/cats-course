package Part3datamanipulation

import cats.Id
import cats.data.WriterT

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Writers {

  import cats.data.Writer

  // 1- define them at the start
  val aWriter: Writer[List[String], Int] = Writer(List("Started Something"), 55)
  // 2- manipulate them with pure Functional Programming
  val anIncrementedWriter: WriterT[Id, List[String], Int] = aWriter.map(_ + 1)
  val aLogsWriter: WriterT[Id, List[String], Int] = aWriter.mapWritten(_ :+ "Found something interesting")
  val aWriterWithBoth: WriterT[Id, List[String], Int] = aWriter.bimap(_ :+ "Found something interesting", _ + 1)
  val aWriterWithBoth2: WriterT[Id, List[String], Int] = aWriter.mapBoth { (logs, value) =>
    (logs :+ "Found something interesting", value + 1)
  }

  import cats.instances.vector._

  val writerA: WriterT[Id, Vector[String], Int] = Writer(Vector("Log A1", "Log A2"), 10)
  val writerB: WriterT[Id, Vector[String], Int] = Writer(Vector("Log B1"), 50)
  val compositeWriter: WriterT[Id, Vector[String], Int] = for {
    valueA <- writerA
    valueB <- writerB
  } yield valueA + valueB

  // reset the logs

  import cats.instances.list._ // an implicit Monoid[List[Int]]

  val anEmptyWriter: WriterT[Id, List[String], Int] = aWriter.reset

  // 3 - dump either the value of the logs
  val desiredValue: Id[Int] = aWriterWithBoth.value
  val logs: Id[List[String]] = aWriterWithBoth.written
  val (l, v) = aWriterWithBoth.run

  // exercise
  def countAndSay(n: Int): Unit =
    if (n <= 0) println("Starting!")
    else {
      countAndSay(n - 1)
      println(n)
    }
  // Benefit : pure FP
  def countAndLog(n: Int): Writer[Vector[String], Int] =
    if (n <= 0) Writer(Vector("Starting"), 0)
    else countAndLog(n - 1).bimap(_ :+ n.toString, _ + 1)


  // exercise
  def naiveSum(n: Int): Int = {
    if (n <= 0) 0
    else {
      println(s"Now at $n")
      val lowerSum = naiveSum(n - 1)
      println(s"Computed sum(${n - 1}) = $lowerSum")
      lowerSum + n
    }
  }

  // Benefit : writers keep logs independent of each other when processed in different threads
  def sumWithLogs(n: Int): Writer[Vector[String], Int] =
    if (n <= 0) Writer(Vector(), 0)
    else
      for {
        _ <- Writer(Vector(s"Now at $n"), n)
        lowerSum <- sumWithLogs(n - 1)
        _ <- Writer(Vector(s"Computed sum(${n - 1}) = $lowerSum"), n)
      } yield lowerSum + n

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(4))

  def main(args: Array[String]): Unit = {
    println(compositeWriter)
    countAndSay(5)
    println(countAndLog(5))
    println(sumWithLogs(5))

    Future(naiveSum(5)).foreach(println)
    Future(naiveSum(5)).foreach(println)

    val future1 = Future(sumWithLogs(5))
    val log1 = future1.map(_.written)
    val future2 = Future(sumWithLogs(5))
    val log2 = future2.map(_.written)

  }
}
