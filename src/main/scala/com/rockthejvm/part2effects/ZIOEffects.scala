package com.rockthejvm.part2effects

import zio.*

import scala.annotation.tailrec
import scala.io.StdIn

object ZIOEffects {

  //success
  val meaningOfLife: ZIO[Any, Nothing, Int] = ZIO.succeed(42)
  //failure
  val aFaliure: ZIO[Any, String, Nothing] = ZIO.fail("Something went wrong")
  //suspension/delay
  val aSuspendedZIO: ZIO[Any, Throwable, Int] = ZIO.suspend(meaningOfLife)

  //map + flatMap
  val improvedMOL = meaningOfLife.map(_ * 2)
  val printingMOL = meaningOfLife.flatMap(mol => ZIO.succeed(println(mol)))

  //for comprehensions
  val smallProgram = for {
    _ <- ZIO.succeed(println("what's your name?"))
    name <- ZIO.succeed(StdIn.readLine())
    _ <- ZIO.succeed(println(s"Welcome to ZIO, $name"))
  } yield ()

  //A LOT of combinators
  //zip, zipWith
  val anotherMOL = ZIO.succeed(100)
  val tupledZio = meaningOfLife.zip(anotherMOL)

  val combinedZIO = meaningOfLife.zipWith(anotherMOL)(_ * _)

  //Type aliases of ZIO

  //UIO[A] == ZIO[Any,Nothing,A] -- no requirements, cannot fail, produces A
  val aUIO: UIO[Int] = ZIO.succeed(99)

  //URIO[R,A] == ZIO[R, Nothing, A] - cannot fail
  val aURIO: URIO[Int, Int] = ZIO.succeed(67)

  //RIO[R,A] = ZIO[Any,E,A] - can fail with a throwable
  val anRIO: RIO[Int, Int] = ZIO.succeed(98)
  val aFailedRIO: RIO[Int, Int] = ZIO.fail(new RuntimeException("RIO failed"))

  // Task[A] == ZIO[Any,Throwable,A] - no requirements, can fail with a Throwable, produces A
  val aSuccessfulTask: Task[Int] = ZIO.succeed(89)
  val aFailedTask: Task[Int] = ZIO.fail(new RuntimeException("Something bad"))

  //IO[E, A] == ZIO[Any, E, A] - no requirements
  val aSuccessfulIO: IO[String, Int] = ZIO.succeed(34)
  val aFailedIO: IO[String, Int] = ZIO.fail("Something bad happened")


  //EXERCISES

  //1 (chains)
  def sequenceTakeLast[R, E, A, B](zioa: ZIO[R, E, A], ziob: ZIO[R, E, B]): ZIO[R, E, B] = {
    zioa.flatMap(a => ziob.map(b => b))
  }

  //(alternative built in method) --sequencing operator
  def sequenceTakeLast_v2[R, E, A, B](zioa: ZIO[R, E, A], ziob: ZIO[R, E, B]): ZIO[R, E, B] = {
    zioa *> ziob
  }

  //2 (for comp)
  def sequenceTakeFirst[R, E, A, B](zioa: ZIO[R, E, A], ziob: ZIO[R, E, B]): ZIO[R, E, A] = for {
    first <- zioa
    _ <- ziob
  } yield first

  //(alternative built in method) --sequencing operator
  def sequenceTakeFirst_v2[R, E, A, B](zioa: ZIO[R, E, A], ziob: ZIO[R, E, B]): ZIO[R, E, A] = {
    zioa <* ziob
  }

  def runForever[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, Nothing] = {
    zio *> runForever(zio)
  }

  //4
  def convert[R, E, A, B](zio: ZIO[R, E, A], value: B): ZIO[R, E, B] = {
    zio.map(a => value)
  }

  //as method does the exact same calculation as above (map)
  def convert_v2[R, E, A, B](zio: ZIO[R, E, A], value: B): ZIO[R, E, B] = {
    zio.as(value)
  }

  //5
  def asUnit[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, Unit] = {
    zio.map(a => ())
  }

  def asUnit_v2[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, Unit] = {
    convert(zio, ())
  }

  //built in method to make a zio value unit
  def asUnit_v3[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, Unit] = {
    zio.unit
  }

  //6 - recursion

  def sumZIO(n: Int): UIO[Int] = {
    if (n == 0) ZIO.succeed(0)
    else for {
      current <- ZIO.succeed(n)
      prevSum <- sumZIO(n - 1)
    } yield current + prevSum
  }

  //7 - fibonacci

//  def fibZIO(n: Int): UIO[BigInt] = {
//    def fib(currentFib: BigInt, prevFib: BigInt, acc: Int): UIO[BigInt] = {
//      if (acc == 0) ZIO.succeed(currentFib)
//      else ZIO.suspendSucceed {
//        for {
//          newFib <- ZIO.succeed(currentFib + prevFib)
//          prevSum <- fib(newFib, currentFib, acc - 1)
//        } yield newFib + prevSum
//      }
//    }
//
//    fib(0, 1, n)
//  }


  //SOLUTIONS

  //3
  def runForever_v2[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] = {
    zio.flatMap(_ => runForever_v2(zio))
  }

  val endlessLoop = runForever_v2 {
    ZIO.succeed {
      println("running...")
      Thread.sleep(1000)
    }
  }

  //7
  def fiboZIO(n: Int): UIO[BigInt] = {
    if (n <= 2) ZIO.succeed(1)
    else for {
      last <- ZIO.suspendSucceed(fiboZIO(n - 1))
      prev <- fiboZIO(n - 2)
    } yield last + prev
  }

  def main(args: Array[String]): Unit = {

    //unsafe api to forcefully evaluate ZIO's to test code.
    val runtime = Runtime.default

    given trace: Trace = Trace.empty

    Unsafe.unsafeCompat { (u: Unsafe) =>
      given uns: Unsafe = u

      val firstEffect = ZIO.succeed {
        println("computing first effect...")
        Thread.sleep(1000)
        1
      }

      val secondEffect = ZIO.succeed {
        println("computing second effect...")
        Thread.sleep(1000)
        2
      }

      println(runtime.unsafe.run(fiboZIO(5)))
    }
  }
}

