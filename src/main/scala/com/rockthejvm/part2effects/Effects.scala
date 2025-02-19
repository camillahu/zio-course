package com.rockthejvm.part2effects

import java.time.OffsetDateTime
import scala.concurrent.Future
import scala.io.StdIn

object Effects {

  // functional programming
  // EXPRESSIONS instead of instructions -- basic fp concept

  def combine(a: Int, b: Int): Int = a + b


  //local reasoning = type signature describes the kind og computation that will be preformed.

  //referential transparency = the ability to replace an expression with the value that it evaluates to.
    //MUST be done without changing the behavior of the program!
    val five = combine(2, 3)
    val five_v2 = 2 + 3
    val five_v3 = 5

  // not all expressions are referentially transparent:
    // example 1: printing
    val resultOfPrintingSomething: Unit = println("Learning ZIO")
    val resultOfPrintingSomething_v2: Unit = () //not the same -- changes the behavior of the program

    //example 2: changing a variable
    var anInt = 0
    var changingInt: Unit = anInt = 42 // side effect -- changing the value stored in variable.
    val changingInt_v2: Unit = () // not the same program
    //this is why we should avoid side effects if we can, but side effects are often inevitable.

  /*
    Effects bridge the gap between RT and side effects.
    Effect desires
    - the type signature describes what KIND of computation it will preform (local reasoning)
    - the type signature describes the type of VALUE that it will produce (local reasoning)
    - if side effects are required, CONSTRUCTION must be separated from the EXECUTION (RT)
   */

  /*
  Example: Option
    - Type signature describes the kind of computation = an Option is a possibly absent value
    - Type signature says that the computation returns an A, if the computation does produce something
    - No side effects are needed

    => OPTION IS AN EFFECT
   */
  val anOption: Option[Int] = Option(42)

  /*
    Example: Future
    - describes an async computation
    - produces a value of type A, if it finishes and it's successful
    - side effects are required, and IT'S NOT separate from execution.

    => FUTURE IS NOT AN EFFECT
   */


  import scala.concurrent.ExecutionContext.Implicits.global //only for example, not recommended global for fp
  val aFuture: Future[Int] = Future(42) // side effects are required - allocating a JVM thread and scheduling.

  /*
    Example 3: MyIO
    - describes a computation which might preform side effects
    - produces a value of type A if the computation is successful
    - side effects are required, but construction IS SEPARATE from execution of said side effect
   */

  case class MyIO[A](unsafeRun: () => A) {
    def map[B](f: A => B): MyIO[B] =
      MyIO(() => f(unsafeRun()))

    def flatMap[B](f: A => MyIO[B]): MyIO[B] =
      MyIO(() => f(unsafeRun()).unsafeRun())

  }

  val anIOWithSideEffects: MyIO[Int] = MyIO(() => {
    println("producing effect")
    42
  }) //will not be printed unless called in main, so it's a valid effect
    // the value only describes the effect, it doesn't execute it

  //EXERCISES
  //1
  val measureSystemTimeIO: MyIO[Long] = MyIO(() => {
    System.currentTimeMillis()
  })

  //2
  def measure[A](computation: MyIO[A]): MyIO[(Long, A)] = {
   measureSystemTimeIO.flatMap(startTime =>
      computation.flatMap(result =>
        measureSystemTimeIO.map(endTime => (endTime - startTime, result))
      )
    )
  }

  def measure_v2[A](computation: MyIO[A]): MyIO[(Long, A)] = for {
    startTime <- measureSystemTimeIO
    result <- computation
    endTime <- measureSystemTimeIO
  } yield (endTime - startTime, result)

  def printIO[A](io: MyIO[A]): MyIO[Unit] = {
    io.flatMap(result => MyIO(() => println(result)))
  }

  //4
  val welcomeProgram: MyIO[Unit] = MyIO(() => {
    println("Whats your name?")
    val s = readLine.unsafeRun()
    println(s"welcome $s")
  })

  //SOLUTIONS
  //3
  def readLine: MyIO[String] = MyIO(() => StdIn.readLine())

//    - the
//  type signature
//  describes what KIND of computation it will preform (local reasoning)
//    - the
//  type signature
//  describes the
//  type of
//  VALUE that it will produce(local reasoning)
//  -
//  if side effects are required
//  , CONSTRUCTION must be separated from the EXECUTION(RT)


  def main(args: Array[String]): Unit = {
//    val aComputation = MyIO (() => combine(2, 3))
//    val measuredComputation = measure_v2(aComputation)
//    val printMC = printIO(measuredComputation)
//    printMC.unsafeRun()

    welcomeProgram.unsafeRun()
  }
}
