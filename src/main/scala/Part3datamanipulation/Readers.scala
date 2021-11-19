package Part3datamanipulation

import cats.Id

import java.io

object Readers {

  /*
      - a configuration file => initial data structure
      - a db layer
      - an HTTP layer
      - a business layer
   */
  case class Configuration(dbUsername: String, dbPassword: String, host: String, port: Int, nThreads: Int, emailReplyTo: String)

  case class DbConnection(username: String, pass: String) {
    def getOrderStatus(orderId: Long): String = "Dispatched" // mock implementation

    def getLastOrderID(userName: String): Long = 789456 // mock implementation
  }

  case class HttpService(host: String, port: Int) {
    def start(): Unit = println("server started") // mock implementation
  }

  // bootstrap
  val config: Configuration = Configuration("daniel", "rockthejvm1!", "localhost", 1234, 8, "store@email.com")

  // cats reader

  import cats.data.Reader

  val dbReader: Reader[Configuration, DbConnection] = Reader(conf => DbConnection(conf.dbUsername, conf.dbPassword))
  val dbConn: DbConnection = dbReader.run(config)

  // Reader[Input, Output]
  val carlosOrderStatusReader: Reader[Configuration, String] = dbReader.map(dbCon => dbCon.getOrderStatus(55))
  val carlosOrderStatus: String = carlosOrderStatusReader.run(config)

  def getLastOrderStatus(username: String): String = {
    val usersLastOrderIdReader: Reader[Configuration, String] = dbReader
      .map(_.getLastOrderID(username))
      .flatMap(lastOrderId => dbReader.map(_.getOrderStatus(lastOrderId)))

    val usersLastOrderIdReaderFor = for {
      orderID <- dbReader.map(_.getLastOrderID(username))
      status <- dbReader.map(_.getOrderStatus(orderID))
    } yield status

    usersLastOrderIdReaderFor.run(config)
  }

  /*
      PATTERN
      1- you create the initial data structure
      2- you create a reader which specifies how that data structure will be manipulated later
      3- you can then map & flatMap the reader to produce derived information
      4- when you need the final piece of information, you call run on the reader with the initial data structures
   */

  // exercise
  case class EmailService(emailReplyTo: String) {
    def sendEmail(address: String, contents: String) = s"From : $emailReplyTo; to :$address >>> $contents"
  }

  def emailUser(userName: String, userEmail: String): String = {
    val emailServiceReader: Reader[Configuration, EmailService] = Reader(conf => EmailService(conf.emailReplyTo))
    val contentForEmail: String => String = (status: String) => s"Your last order has the status : $status"

    val sendEmailReaderFor: Reader[Configuration, String] = for {
      orderID <- dbReader.map(_.getLastOrderID(userName))
      status <- dbReader.map(_.getOrderStatus(orderID))
      email <- emailServiceReader.map(_.sendEmail(userEmail, contentForEmail(status)))
    } yield email

    sendEmailReaderFor.run(config)
  }

  def main(args: Array[String]): Unit = {
    println(getLastOrderStatus("CR"))

    println(emailUser("CR", "cr@email.com"))

  }

}
