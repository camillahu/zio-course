package com.rockthejvm.part2effects

import zio._

object ZIOExercises2 {

  //1
  def sequenceTakeLast[R,E,A,B](zioa: ZIO[R,E,A], ziob: ZIO[R,E,B]): ZIO[R,E,B] = {
    for{
      a <- zioa
      b <- ziob
    } yield b
  }

  //3
  def runForever[R,E,A](zio: ZIO[R,E,A]): ZIO[R,E,A] = {

    val endlessLoop = runForever {
      ZIO.succeed {
        println("running...")
        Thread.sleep(1000)
      }
    }
    zio.flatMap(_ => runForever(zio))
  }

  //4

  def convert[R,E,A,B](zio: ZIO[R,E,A], value: B): ZIO[R, E, B] = {
    zio.map(_ => value)
  }

  //5
  def asUnit[R,E,A](zio: ZIO[R,E,A]): ZIO[R,E, Unit] = {
    convert(zio, ())
  }

  def main(args: Array[String]): Unit = {
    val runtime = Runtime.default

    implicit val trace: Trace = Trace.empty

    val first: ZIO[Any, Any, Int] = sequenceTakeLast(ZIO.succeed(1), ZIO.succeed(2))

    val convertVal = convert(ZIO.succeed(1), 3)
    val unitVal = asUnit(ZIO.succeed(4))

    Unsafe.unsafeCompat { (u: Unsafe) =>
      given uns: Unsafe = u

      println(runtime.unsafe.run(unitVal))
    }
  }
}
