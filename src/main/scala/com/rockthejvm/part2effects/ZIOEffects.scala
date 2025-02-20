package com.rockthejvm.part2effects

import zio.*

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
  //TODO

  //1
  def sequenceTakeLast[R, E, A, B](zioa: ZIO[R, E, A], ziob: ZIO[R, E, B]): ZIO[R, E, B] = {
    zioa.flatMap(_ => ziob)
  }

  //2
  def sequenceTakeFirst[R, E, A, B](zioa: ZIO[R, E, A], ziob: ZIO[R, E, B]): ZIO[R, E, A] = for {
    first <- zioa
    _ <- ziob
  } yield first

  //3
  def runForever[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] = {
    val endlessLoop = runForever {
      ZIO.succeed {
        println("running...")
        Thread.sleep(1000)
      }
    }
    endlessLoop match {
      case _ => runForever(zio)
    }
  }




  def main(args: Array[String]): Unit = {

    val e1 = sequenceTakeLast(meaningOfLife, improvedMOL)
    val e2 = sequenceTakeFirst(meaningOfLife, improvedMOL)

    //unsafe api to forcefully evaluate ZIO's to test code.
    val runtime = Runtime.default
    implicit val trace: Trace = Trace.empty
    Unsafe.unsafe { implicit u: Unsafe =>
      val mol = runtime.unsafe.run(e2)
      println(mol)
    }
  }
}
