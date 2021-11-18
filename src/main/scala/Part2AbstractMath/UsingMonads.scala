package Part2AbstractMath

object UsingMonads {

  // recap

  import cats.Monad
  import cats.instances.list._
  import cats.instances.option._

  val monadList: Monad[List] = Monad[List]
  val aSimpleList: List[Int] = monadList.pure(5)
  val aTransformedList: List[Int] = monadList.flatMap(aSimpleList)(x => List(x, x + 1))

  // Either
  val aManualMonad: Either[String, Int] = Right(55)
  type LoadingOr[T] = Either[String, T]
  type ErrorOr[T] = Either[Throwable, T]

  import cats.instances.either._

  val loadingMonad: Monad[LoadingOr] = Monad[LoadingOr]
  val anEither: LoadingOr[Int] = loadingMonad.pure(45)
  val aChangedLoading: LoadingOr[Int] =
    loadingMonad.flatMap(anEither)(n => if (n % 2 == 0) Right(n + 1) else Left("Loading meaning of life"))


  // online store example
  case class OrderStatus(orderId: Long, status: String)

  def getOrderStatus(orderID: Long): LoadingOr[OrderStatus] =
    Right(OrderStatus(orderID, "Ready to ship"))

  def trackStatus(orderStatus: OrderStatus): LoadingOr[String] = {
    if (orderStatus.orderId > 1000) Left("Not available yet, refreshing data...")
    else Right("NL")
  }

  val orderID = 457L
  val orderLocation: LoadingOr[String] =
    loadingMonad.flatMap(getOrderStatus(orderID))(orderStatus => trackStatus(orderStatus))

  // extension methods for comprehension

  import cats.syntax.flatMap._
  import cats.syntax.functor._

  val orderLocationBetter: LoadingOr[String] =
    getOrderStatus(orderID)
      .flatMap(orderStatus => trackStatus(orderStatus))

  val orderLocationShorter: LoadingOr[String] =
    for {
      orderStatus <- getOrderStatus(orderID)
      location <- trackStatus(orderStatus)
    } yield location

  // exercise
  case class Connection(host: String, port: String)

  val config = Map(
    "host" -> "localhost",
    "port" -> "4040"
  )

  trait HttpService[M[_]] {
    def getConnection(cfg: Map[String, String]): M[Connection]

    def issueRequest(connection: Connection, payload: String): M[String]
  }

  def getResponse[M[_]](service: HttpService[M], payload: String)(implicit monad: Monad[M]): M[String] =
    for {
      conn <- service.getConnection(config)
      response <- service.issueRequest(conn, payload)
    } yield response

  object HttpServiceOption extends HttpService[Option] {
    override def getConnection(cfg: Map[String, String]): Option[Connection] = for {
      host <- cfg.get("host")
      port <- cfg.get("port")
    } yield Connection(host, port)

    override def issueRequest(connection: Connection, payload: String): Option[String] =
      if (payload.length > 20) None
      else Some(s"Request ($payload) has been accepted")
  }

  val responseOption: Option[String] = HttpServiceOption.getConnection(config).flatMap(conn => HttpServiceOption.issueRequest(conn, "Hello World"))
  val responseOptionFor: Option[String] = for {
    conn <- HttpServiceOption.getConnection(config)
    response <- HttpServiceOption.issueRequest(conn, "allo")
  } yield response

  object HttpServiceErrorOR extends HttpService[ErrorOr] {
    override def getConnection(cfg: Map[String, String]): ErrorOr[Connection] = {
      val maybeConn = for {
        host <- cfg.get("host")
        port <- cfg.get("port")
      } yield Connection(host, port)

      maybeConn match {
        case Some(conn) => Right(conn)
        case None => Left(new RuntimeException("Failed to get connection!"))
      }
    }

    override def issueRequest(connection: Connection, payload: String): ErrorOr[String] =
      if (payload.length > 20) Left(new RuntimeException("Invalid Payload!"))
      else Right(s"Request ($payload) has been accepted")
  }

  val responseErrorOr: ErrorOr[String] = HttpServiceErrorOR.getConnection(config).flatMap(conn => HttpServiceErrorOR.issueRequest(conn, "Hello World"))
  val responseErrorOrFor: ErrorOr[String] = for {
    conn <- HttpServiceErrorOR.getConnection(config)
    response <- HttpServiceErrorOR.issueRequest(conn, "allo")
  } yield response


  def main(args: Array[String]): Unit = {
    println(responseOptionFor)
    println(responseErrorOrFor)

    println(getResponse(HttpServiceOption,"allo")) // because import cats.instances.option._
    println(getResponse(HttpServiceErrorOR,"allo"))// because import cats.instances.either._

  }
}
