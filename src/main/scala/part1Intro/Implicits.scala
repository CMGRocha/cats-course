package part1Intro

object Implicits {

  // implicit classes
  case class Person(name: String) {
    def greet: String = s"Hi, my name is $name"
  }

  implicit class ImpersonableString(name: String) {
    def greet: String = Person(name).greet
  }
  //    val impersonableString = new ImpersonableString("Peter")
  //    impersonableString.greet

  val greeting: String = "Peter".greet // impersonableString = new ImpersonableString("Peter").greet

  //import implicit conversions in scope

  import scala.concurrent.duration._

  val oneSec: FiniteDuration = 1.second

  // implicit arguments and values
  def increment(x: Int)(implicit amount: Int): Int = x + amount

  implicit val defaultAmount: Int = 10
  val increment2: Int = increment(5)


  trait JSONSerializer[T] {
    def toJson(value: T): String
  }

  def listToJson[T](list: List[T])(implicit serializer: JSONSerializer[T]): String =
    list.map(value => serializer.toJson(value)).mkString("[", ",", "]")

  implicit val personSerializer: JSONSerializer[Person] = new JSONSerializer[Person] {
    override def toJson(person: Person): String =
      s"""
         |{"name" : "${person.name}"}
         |""".stripMargin
  }

  val personJson = listToJson(List(Person("Alice"), Person("Bob")))
  // implicit argument is used to prove the existence of a type

  // implicit methods
  implicit def oneArgCaseClassSerializer[T <: Product]: JSONSerializer[T] = new JSONSerializer[T] {
    override def toJson(value: T): String =
      s"""
         |{"${value.productElementName(0)}" : "${value.productElement(0)}"}
         |""".stripMargin.trim
  }

  case class Cat(name: String)

  val catsToJson: String = listToJson(List(Cat("Tom"), Cat("Garfield")))
  // implicit methods are used to prove the existence of a type


  def main(args: Array[String]): Unit = {
    println(oneArgCaseClassSerializer[Cat].toJson(Cat("Garfield")))
    println(oneArgCaseClassSerializer[Person].toJson(Person("Carlos")))

  }

}
