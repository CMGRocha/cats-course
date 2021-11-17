package part1Intro

object TCVariance {

  import cats.Eq
  import cats.instances.int._ // EQ[Int] Type class instance
  import cats.instances.option._ // construct Eq[Optional[Int]] type class instance
  import cats.syntax.eq._

  val aComparison: Boolean = Option(2) === Option(3)
  // val aInvalidComparison = Some(2) === None // Eq[Some[Int]] not found
  // val aInvalidComparison2: Boolean = Some(2) === Some(3)

  // variance
  class Animal

  class Cat extends Animal

  // covariant type : subtyping is propagated to the generic type
  class Cage[+T]

  val cage: Cage[Animal] = new Cage[Cat] // Cat <: Animal, so Cage[Cat] <: Cage[Animal]

  // contravariant type : subtyping is propagated backwards to the generic type
  // contravariant associated with actions
  class Vet[-T]

  val vet: Vet[Cat] = new Vet[Animal] // Cat <: Animal, so Vet[Animal] <: Vet[Cat]
  // i need a vet of cat i need a vet that heal any animal


  // Rule of thumb
  // "HAS a T" / "Contains a T" = Covariant
  // "ACTS on T" / "Operates on T" = Contravariant

  // variance affect how type classes are being fetched

  // Contravariant Type Class
  trait SoundMaker[-T]

  implicit object AnimalSoundMaker extends SoundMaker[Animal]

  def makeSound[T](implicit soundMaker: SoundMaker[T]): Unit = println("WOW") // random implementation

  makeSound[Animal] // OK - type class instance implemented above
  makeSound[Cat] // OK - Type class instance for Animal is also applicable to Cats
  // RULE 1 - contravariant Type classes can use the super class instances if nothing is available strictly for that type

  // impacts subtypes
  implicit object OptionIntSoundMaker extends SoundMaker[Option[Int]]
  makeSound[Option[Int]]
  makeSound[Some[Int]]

  // Covariant type class
  trait AnimalShow[+T]{
    def show: String
  }

  implicit object GeneralAnimalShow extends AnimalShow[Animal]{
    override def show: String = "animals everywhere"
  }

  implicit object CatsShow extends AnimalShow[Cat] {
    override def show: String = "so many cats!"
  }

  def organizeShow[T](implicit event : AnimalShow[T]): String = event.show
  // rule 2 : covariant TC's will always use the more specific Type Class instance for that type
  // but may confuse the compiler if the general Type Class is also present

  organizeShow[Cat] //  OK the compiler will inject CatsShow as implicit
  // organizeShow[Animal] // will not compile -> ambiguous values (see rule #2)


  // Rule 3 : you can't have both benefits
  // cats uses invariant type classes
  // val aInvalidComparison = Some(2) === None // Eq[Some[Int]] not found
  val theAlternative : Boolean = Option(3) === Option.empty[Int]
}
