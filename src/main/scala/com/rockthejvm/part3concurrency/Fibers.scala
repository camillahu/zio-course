package com.rockthejvm.part3concurrency

import zio.*
import com.rockthejvm.utils.*

import java.io.{File, FileWriter}

object Fibers extends ZIOAppDefault {

  //PARALLELISM VS CONCURRENCY:

  /*
  Two computations are parallel if they run at the same time
  Two computations are concurrent if their lifecycles overlap

  Parallel computations may not be concurrent
  - Independent tasks, don't share resources

  Concurrent computations may not be parallel
  - Multi-tasking on the same CPU with resource sharing
  */

  val meaningOfLife = ZIO.succeed(42)
  val favLang = ZIO.succeed("Scala")

  // Fiber = lightweight thread
  // Description of a computation that will be preformed by one of the threads managed by the ZIO runtime.
  // Basic difference between threads and fibers: Threads on JVM, fibers on ZIO.

  def createFiber: Fiber[Throwable, String] = ??? //impossible to create manually- use Scala API.

  //sequential execution -- the contents of for comp will always run on the same thread
  val sameThreadIO = for {
    mol <- meaningOfLife.debugThread
    lang <- favLang.debugThread
  } yield (mol,lang)

  //fork produces an effect whos value is a fiber.
  //the contents of for comp will always run on the different threads in this example:
  val differentThreadIO = for {
    _ <- meaningOfLife.debugThread.fork
    _ <- favLang.debugThread.fork
  } yield ()

  val meaningOfLifeFiber: ZIO[Any, Nothing, Fiber[Throwable, Int]] = meaningOfLife.fork

  //join a fiber
  def runOnAnotherThread[R,E,A](zio: ZIO[R,E,A]) = for {
    fib <- zio.fork
    result <- fib.join //join returns another effect that will block until fiber is complete
  } yield result

  //awaiting a fiber --exposes the exit datastructure which exposes defects if failure.
  def runOnAnotherThread_v2[R,E,A](zio: ZIO[R,E,A]) = for {
    fib <- zio.fork
    result <- fib.await
  } yield result match {
    case Exit.Success(value) => s"succeded with $value"
    case Exit.Failure(cause) => s"failed with $cause"
  }

  //poll - peek at the result of a fiber RIGHT NOW, without blocking
  val peekFiber = for {
    fib <- ZIO.attempt {
      Thread.sleep(1000)
      42
    }.fork
    result <- fib.poll
  }yield result

  //compose fibers -- result are both of these fibers when they complete -- zip
  val zippedFibers = for {
    fib1 <- ZIO.succeed("result from fiber 1").debugThread.fork
    fib2 <- ZIO.succeed("result from fiber 2").debugThread.fork
    fiber = fib1.zip(fib2)
    tuple <- fiber.join
  } yield tuple

  //chain fibers -- orElse
  val chainedFibers = for {
    fiber1 <- ZIO.fail("not good").debugThread.fork
    fiber2 <- ZIO.succeed("Rock the JVM!").debugThread.fork
    fiber = fiber1.orElse(fiber2)
    message <- fiber.join
  } yield message

  //Exercises

  //1
  def zipFibers[E,A,B](fiber1: Fiber[E,A], fiber2: Fiber[E,B]): ZIO[Any, Nothing, Fiber[E, (A,B)]] = {
    val finalEffect = for {
      v1 <- fiber1.join
      v2 <- fiber2.join
    } yield (v1, v2)
    finalEffect.fork
  }

  def zipFibersGeneral[E, E1 <: E, E2 <: E, A, B](fiber1: Fiber[E1, A], fiber2: Fiber[E2, B]): ZIO[Any, Nothing, Fiber[E, (A, B)]] = {
    //same implementation but can take different value types.
    val finalEffect = for {
      v1 <- fiber1.join
      v2 <- fiber2.join
    } yield (v1, v2)
    finalEffect.fork
  }

  val zippedFibers_v2 = for {
    fib1 <- ZIO.succeed("result from fiber 1").debugThread.fork
    fib2 <- ZIO.succeed("result from fiber 2").debugThread.fork
    fiber <- zipFibers(fib1, fib2)
    tuple <- fiber.join
  } yield tuple

  // 2
  def chainFibers[E,A](fiber1: Fiber[E,A], fiber2: Fiber[E,A]): ZIO[Any, Nothing, Fiber[E, A]] = {
    val waitFiber1 = fiber1.join
    val waitFiber2 = fiber2.join
    val finalEffect = waitFiber1.orElse(waitFiber2)
    finalEffect.fork
  }

  def chainFibers_v2[E, A](fiber1: Fiber[E, A], fiber2: Fiber[E, A]): ZIO[Any, Nothing, Fiber[E, A]] = {
    fiber1.join.orElse(fiber2.join).fork
  }


  //3
  def generateRandomFile(path: String): Unit = {
    val random = scala.util.Random
    val chars = 'a' to 'z'
    val nWords = random.nextInt(2000) //at most 2000 words

    val content = (1 to nWords)
      .map(n =>
        (1 to random.nextInt(10)).map(_ => chars(random.nextInt(26))).mkString).mkString(" ")
    // one word for every 1 to nWords

    val writer = new FileWriter(new File(path))
    writer.write(content)
    writer.flush()
    writer.close()
  }

  //part 1 - an effect that reads one file and counts the words there
  def countWords(path: String): UIO[Int] = {
    ZIO.succeed {
      val source = scala.io.Source.fromFile(path)
      val nWords = source.getLines().mkString(" ").split(" ").count(_.nonEmpty)
      println(s"Counted $nWords in $path")
      source.close()
      nWords
    }
  }

  //part 2 - spin up fibers for all the files
  def wordCountParallel(n: Int): UIO[Int] = {
    val effects: Seq[ZIO[Any, Nothing, Int]] = (1 to n)
      .map(i => s"src/main/resources/testfile_$i.txt") //paths
      .map(countWords) //list of effects
      .map(_.fork) //list of effects returning fibers
      .map((fiberEff: ZIO[Any, Nothing, Fiber[Nothing, Int]]) => fiberEff.flatMap(_.join)) //list of effects returning values(count of words)

    effects.reduce{ (zioa, ziob) =>
      for {
        a <- zioa
        b <- ziob
      } yield a + b
    }
  }

  def run = wordCountParallel(10).debugThread

    //ZIO.succeed((1 to 10).foreach(i => generateRandomFile(s"src/main/resources/testfile_$i.txt")))
}
