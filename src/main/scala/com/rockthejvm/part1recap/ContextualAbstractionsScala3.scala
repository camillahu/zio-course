package com.rockthejvm.part1recap

object ContextualAbstractionsScala3 {

  //given/using combo -- equivalent to implicit args/values in scala 2
  def increment(x:Int)(using amount: Int): Int = x + amount
  given defaultAmount: Int = 10 // <-- whenever you need an int with the using keyword, compiler will pass this arg.
  val twelve: Int = increment(2) // (10)

  def multiply(x: Int)(using factor: Int): Int = x * factor
  val aHundred: Int = multiply(10) // default amount passed automatically

  //more complex use case
  trait Combiner[A] {
    def combine(x:A, y:A): A
    def empty: A
  }

  def combineAll[A](values: List[A])(using combiner: Combiner[A]): A =
    values.foldLeft(combiner.empty)(combiner.combine)

  given intCombiner: Combiner[Int] with {
    override def combine(x: Int, y: Int) = x + y
    override def empty = 0
  }
  
  private val numbers = (1 to 10).toList
  val sum10: Int = combineAll(numbers) 

  //synthesize given instances
  given optionCombiner[T](using combiner: Combiner[T]): Combiner[Option[T]] with {
    override def empty = Some(combiner.empty)
    override def combine(x: Option[T], y:Option[T]): Option[T] = for {
      vx <- x
      vy <- y
    } yield combiner.combine(vx, vy)
  }

  val sumOptions: Option[Int] = combineAll(List(Some(1), None, Some(2)))

  //extension methods
  case class Person(name:String) {
    def greet(): String = s"Hi, my name is $name"
  }

  extension (name: String)
    def greet(): String = Person(name).greet()

  val aliceGreeting: String = "Alice".greet()

  //generic extension
  extension [T](list: List[T])
    def reduceAll(using combiner: Combiner[T]): T =
      list.foldLeft(combiner.empty)(combiner.combine)

  val sum10_v2: Int = numbers.reduceAll

  def main(args: Array[String]): Unit = {

  }
}
