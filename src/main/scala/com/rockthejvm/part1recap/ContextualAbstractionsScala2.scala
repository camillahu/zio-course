package com.rockthejvm.part1recap

object ContextualAbstractionsScala2 {

  //implicit classes
  case class Person(name: String) {
    def greet(): String = s"Hi, my name is $name"
  }

  implicit class ImpersonableString(name: String) {
    def greet(): String =
      Person(name).greet()
  } // <-- implicit classes are a way to add methods to existing classes or datatypes.

  //extension method
  val greeting: String = "Peter".greet() // new ImpersonableString("Peter").greet()

  // example: scala.concurrent.duration
  import scala.concurrent.duration._ // <--- a popular extension method available in the scala library.
  val oneSecond: FiniteDuration = 1.second

  // implicit arguments and values
  private def increment(x:Int)(implicit amount: Int) = x + amount
  implicit val defaultAmount: Int = 10 // <-- will be used whenever we require an implicit arg of that type in scope.
  val twelve: Int =  increment(2) //implicit argument 10 passed by the compiler.

  private def multiply(x: Int)(implicit factor: Int) = x * factor
  val aHundred: Int = multiply(10) //same implicit argument passed by the compiler

  //more complex example
  trait JSONSerializer[T] {
    def toJson(value:T): String
  }

  private def convert2Json[T](value: T)(implicit serializer: JSONSerializer[T]): String =
    serializer.toJson(value)

  implicit val personSerializer: JSONSerializer[Person] = new JSONSerializer[Person] {
    override def toJson(person: Person): String = "{\"name\" : \"" + person.name + "\"}"
  }

  private val davidJson = convert2Json(Person("David")) // implicit serializer passed here

  // implicit defs:

  //a def that can serialize anything if we have a serializer of that particular type:
  implicit def createListSerializer[T](implicit serializer: JSONSerializer[T]): JSONSerializer[List[T]] = {
    new JSONSerializer[List[T]] {
      override def toJson(list: List[T]) = s"[${list.map(serializer.toJson).mkString(", ")}]"
    }
  }

  val personJson: String = convert2Json(List(Person("Alice"), Person("Bob")))

  def main(args: Array[String]): Unit = {
    println(davidJson)
    println(personJson)
  }
}
