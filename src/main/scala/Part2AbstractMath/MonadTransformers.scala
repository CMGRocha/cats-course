package Part2AbstractMath

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object MonadTransformers {

  // option transformer

  import cats.data.OptionT
  import cats.instances.list._
  import cats.instances.future._

  val listOfNumberOptions: OptionT[List, Int] = OptionT(List(Option(1), Option(2)))
  val listOfCharOptions: OptionT[List, Char] = OptionT(List(Option('a'), Option('b')))

  val listOfTuples: OptionT[List, (Int, Char)] = for {
    number <- listOfNumberOptions
    char <- listOfCharOptions
  } yield (number, char)

  // either transformer

  import cats.data.EitherT

  val listOfEither: EitherT[List, String, Int] = EitherT(List(Left("something went wrong"), Right(55), Right(5)))
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(4))
  // val futureOfEither: EitherT[Future, String, Int] = EitherT(Future[Either[String, Int]](Right(45))
  val futureOfEither: EitherT[Future, String, Int] = EitherT.right(Future(45))

  // Exercise
  val bandwidths: Map[String, Int] = Map(
    "server1.rockthejvm.com" -> 50,
    "server2.rockthejvm.com" -> 300,
    "server3.rockthejvm.com" -> 170,
  )
  type AsyncResponse[T] = EitherT[Future, String, T]

  def getBandwidth(server: String): AsyncResponse[Int] = bandwidths.get(server) match {
    //    case Some(value) => EitherT(Future[Either[String, Int]](Right(value)))
    //    case None => EitherT(Future[Either[String, Int]](Left(s"Server $server unreachable")))
    case Some(value) => EitherT.right(Future(value))
    case None => EitherT.left(Future(s"Server $server unreachable"))
  }


  def canWithstandSurge(s1: String, s2: String): AsyncResponse[Boolean] =
    for {
      s1Capacity <- getBandwidth(s1)
      s2Capacity <- getBandwidth(s2)
    } yield s1Capacity + s2Capacity > 250

  def generateTrafficSpikeReport(s1: String, s2: String): AsyncResponse[String] = {
    canWithstandSurge(s1, s2).transform {
      case Left(reason) => Left(s"Servers $s1 and $s2 failed due to $reason")
      case Right(false) => Left(s"Servers $s1 and $s2 CANNOT cope with the load")
      case Right(true) => Right(s"Servers $s1 and $s2 can cope with the load")
    }
  }

  def main(args: Array[String]): Unit = {
    println(listOfTuples.value)


    val report: Future[Either[String, String]] = generateTrafficSpikeReport("server1.rockthejvm.com", "server2.rockthejvm.com").value
    report.foreach(println)
  }
}
