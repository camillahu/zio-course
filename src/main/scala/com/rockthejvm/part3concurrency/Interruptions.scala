package com.rockthejvm.part3concurrency

import zio._
import com.rockthejvm.utils._

object Interruptions extends ZIOAppDefault {
  val zioWithTime =
    (
      ZIO.succeed("starting computation").debugThread *>
        ZIO.sleep(2.seconds) *>
        ZIO.succeed(42).debugThread
    )
      .onInterrupt(ZIO.succeed("I was interrupted!").debugThread) //guard for interruptions
      //onInterrupt, onDone(clean up resources) callbacks are available as well


  val interruption = for {
    fib <- zioWithTime.fork
    _ <- ZIO.sleep(1.second) *> ZIO.succeed("Interrupting!").debugThread *> fib.interrupt //can also use interruptFork effect
    //above is an effect, blocks the calling fiber until the interrupted fiber is done/interrupted
    _ <- ZIO.succeed("Interruption successful").debugThread
    result <- fib.join
  } yield result

  //Automatic interruption cases

  //outliving a parent fiber's lifecycle

    val parentEffect =
      ZIO.succeed("spawning fiber").debugThread *>
       // zioWithTime.fork *> // child fiber
        zioWithTime.forkDaemon *> // child fiber
      ZIO.sleep(1.second) *>
        ZIO.succeed("parent succesful").debugThread //parent is done here

    val testOutlivingParent = for {
      parentEffectFib <- parentEffect.fork
      _ <- ZIO.sleep(3.seconds)
      _ <- parentEffectFib.join
    } yield ()
    // child fibers will be automatically interrupted if the parent fiber is completed
    // can be overridden with .forkDaemon -- fiber will now be a child of main application instead and will not
    // be interrupted when original parent is done


  //racing - happens often

    val slowEffect = (ZIO.sleep(2.seconds) *> ZIO.succeed("slow").debugThread).onInterrupt(ZIO.succeed("[slow] interrupted").debugThread)
    val fastEffect = (ZIO.sleep(1.seconds) *> ZIO.succeed("fast").debugThread).onInterrupt(ZIO.succeed("[fast] interrupted").debugThread)
    val aRace = slowEffect.race(fastEffect)
    val testRace = aRace.fork *> ZIO.sleep(3.seconds)

  //exercises

  val interruption = for {
    fib <- zioWithTime.fork
    _ <- ZIO.sleep(1.second) *> ZIO.succeed("Interrupting!").debugThread *> fib.interrupt //can also use interruptFork effect
    //above is an effect, blocks the calling fiber until the interrupted fiber is done/interrupted
    _ <- ZIO.succeed("Interruption successful").debugThread
    result <- fib.join
  } yield result

  //1
  def timeout[R,E,A](zio: ZIO[R,E,A], time: Duration): ZIO[R,E,A] = {
    val fib = zio.fork

    ZIO.sleep(time) *> fib.interrupt
    zio.foldZIO(
      ex => ZIO.fail(ex),
      value => ZIO.succeed(value)
    )
  }


  //2
  def timeout_v2[R,E,A](zio: ZIO[R,E,A], time: Duration): ZIO[R,E,Option[A]] = ???


  def run = testRace
}
