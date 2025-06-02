package com.rockthejvm.part3concurrency

import zio._
import com.rockthejvm.utils._

object Parallelism extends ZIOAppDefault {

  val meaningOfLife = ZIO.succeed(42)
  val favLang = ZIO.succeed("Scala")
  val combined = meaningOfLife.zip(favLang) //sequential combination

  //combine in parallel:
  val combinePar = meaningOfLife.zipPar(favLang)

  /*
  - start each on fibers
  - What if one fails? the other should be interrupted
  - What is one is interrupted? the entire thing should be interrupted
  - What is the whole thing is interrupted? need to interrupt both effects
  * */


  //homemade basic implementation of zipPar
  def myZipPar[R,E,A,B](zioa: ZIO[R,E,A], ziob: ZIO[R,E,B]): ZIO[R,E,(A,B)] = {
    val exits = for {
      fiba <- zioa.fork
      fibb <- ziob.fork
      exita <- fiba.await
      exitb <- exita match {
        case Exit.Success(value) => fibb.await
        case Exit.Failure(_) => fibb.interrupt
      }
    } yield(exita, exitb)

    exits.flatMap {
      case (Exit.Success(a), Exit.Success(b)) => ZIO.succeed((a, b)) // happy path
      case (Exit.Success(_), Exit.Failure(cause)) => ZIO.failCause(cause) // one of them failed
      case (Exit.Failure(cause), Exit.Success(_)) => ZIO.failCause(cause) // one of them failed (this is case will theoretically never happen)
      case (Exit.Failure(c1), Exit.Failure(c2)) => ZIO.failCause(c1 && c2) //both failed
    }
  }

  //parallel combinators
  //zipPar, zipWithPar

  //collectAllPar - takes a collection of ZIOs and returns a ZIO with the values of all the ZIOs in collection
  val effects: Seq[ZIO[Any, Nothing, Int]] = (1 to 10).map(i => ZIO.succeed(i).debugThread)
  val collectedValues: ZIO[Any, Nothing, Seq[Int]] = ZIO.collectAllPar(effects) //"traverse"

  //foreachPar
  val printlnParallel = ZIO.foreachPar((1 to 10).toList)(i => ZIO.succeed(println(i)))

  //reduceAllPar, mergeAllPar
  val sumPar = ZIO.reduceAllPar(ZIO.succeed(0), effects)(_ + _)
  val sumPar_v2 = ZIO.mergeAllPar(effects)(0)(_ + _)

  /*
  - if all the effects succeed, all good
  - one effect fails => everyone else is interrupted, error is surfaced
  - one effect is interrupted => everyone else is interrupted, error = interruption (for the big expression)
  - if the entire thing is interrupted => all effects are interrupted
  * */

  //exercises

  def countWords(path: String): UIO[Int] = {
    ZIO.succeed {
      val source = scala.io.Source.fromFile(path)
      val nWords = source.getLines().mkString(" ").split(" ").count(_.nonEmpty)
      println(s"Counted $nWords in $path")
      source.close()
      nWords
    }
  }

  //1

  def wordCountParallel(n: Int): UIO[Int]  = {
//    val effects: Seq[ZIO[Any, Nothing, Int]] = (1 to n).map(i => ZIO.succeed(i))
    val effects: Seq[ZIO[Any, Nothing, Int]] = (1 to n)
      .map(i => s"src/main/resources/testfile_$i.txt")
      .map(path => countWords(path))

    //collectAllPar
    ZIO.collectAllPar(effects).map(_.sum)

    //mergeAllPar
    ZIO.mergeAllPar(effects)(0)(_ + _)
  }

  def run = wordCountParallel(10).debugThread
}
