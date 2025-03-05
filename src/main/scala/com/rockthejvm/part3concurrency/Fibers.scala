package com.rockthejvm.part3concurrency

import zio._
import com.rockthejvm.utils._

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

  def run = chainedFibers.debugThread

}
