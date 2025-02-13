package com.rockthejvm.part1recap

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

object Essentials {

  //values
  val aBoolean: Boolean = false

  //expressions are EVALUATED to a value
  val anIfExpression = if (2 < 3) "bigger" else "smaller"

  //instructions vs expressions
  val theUnit = println("Hello Scala")

  // OOP
  class Animal
  class Cat extends Animal
  trait Carnivore {
    def eat(animal:Animal): Unit
  }

  //inheritance model: extend at most one class, but inherit from at least zero traits.
  class Crocodile extends Animal with Carnivore {
    override def eat(animal: Animal): Unit = println("crunch")
  }

  //singleton
  object MySingleton

  //companions
  object Carnivore //companion object for the class Carnivore.

  //generics
  class MyList[A]

  //method notation
  val three = 1 + 2
  val anotherThree = 1.+(2)

  //functional programming
  val incrementer: Int => Int = x => x + 1
  val incremented = incrementer(45) // 46

  // map, flatMap, filter
  val aProcessedList = List(1,2,3).map(incrementer) //List(2,3,4)
  val aLongerList = List(1,2,3).flatMap(x => List(x, x + 1)) //List(1,2,2,3,3,4)

  //for-comprehensions
  val checkerboard = List(1, 2, 3).flatMap(n => List('a', 'b', 'c').map(c=>(n, c))) //use for instead

  val anotherCheckerBoard = for {
    n <- List(1,2,3)
    c <- List('a', 'b', 'c')
  } yield (n,c) //equivalent expression

  //options and try
  val anOption = Option(/*something that might be null*/3) // Some(3)
  val doubledOption: Option[Int] = anOption.map(_ * 2)

  val anAttempt = Try(/*something that might throw*/42) // Success(42)
  val aModifiedAttempt:Try[Int] = anAttempt.map(_ + 10)

  //pattern matching
  val anUnknown: Any = 45
  val ordinal = anUnknown match {
    case 1 => "first"
    case 2 => "second"
    case _ => "unknown"
  }

  val optionDescription = anOption match {
    case Some(value) => s"the option is not empty: $value"
    case None => "the option is empty"
  }

  // Futures
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val aFuture = Future {
    //a bit of code
    42
  }
    //wait for completion (async)
  aFuture.onComplete {
    case Success(value) => println(s"The async meaning of life is $value")
    case Failure(exception) => println(s"The meaning of value failed: $exception")
  } //partial function

  // map a Future
  val anotherFuture = aFuture.map(_ +1) //Future(43) when it completes

  //partial functions
  val aPartialFunction: PartialFunction[Int,Int] = {
    case 1 => 43
    case 8 => 56
    case 100 => 999
  }

  //some more advanced stuff
  trait HigherKindedType[F[_]]
  trait SequenceChecker[F[_]] {
    def isSequential: Boolean
  }

  val listChecker = new SequenceChecker[List] {
    override def isSequential: Boolean = true
  }

  def main(args: Array[String]): Unit = {

  }
}
