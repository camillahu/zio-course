package com.rockthejvm.part3concurrency

import zio.*
import com.rockthejvm.utils.*

import java.util.concurrent.atomic.AtomicBoolean

object BlockingEffects extends ZIOAppDefault{

  def blockingTask(n:Int): UIO[Unit] = {
    ZIO.succeed(s"running blocking task $n").debugThread *>
      ZIO.succeed(Thread.sleep(10000)) *>
      blockingTask(n)
  }

  val program = ZIO.foreachPar((1 to 100).toList)(blockingTask)
  // will trigger thread starvation (because the thread sleep takes time) - it will occupy all threads making it lag

  //SOLUTION - a separate blocking thread pool:
  val aBlockingZIO = ZIO.attemptBlocking {
    println(s"[${Thread.currentThread().getName}] running a long computation...")
    Thread.sleep(10000)
    42
  }

  //blocking code cannot usually be interrupted
    //the status of the fiber below will show as interrupted, but the computation has been preformed.
  val tryInterrupting = for {
    blockingFib <- aBlockingZIO.fork
    _ <- ZIO.sleep(1.second) *>  ZIO.succeed("interrupting...").debugThread *> blockingFib.interrupt
    mol <- blockingFib.join
  } yield mol

  //below is based on Thread.interrupt -> InterruptException
    //if this doesn't work, the code is not interruptible at all.
  val aBlockingInterruptableZIO = ZIO.attemptBlockingInterrupt {
    println(s"[${Thread.currentThread().getName}] running a long computation...")
    Thread.sleep(10000)
    42
  }

  //you can achieve interrupting in another way if it's "impossible" with a switch/flag

  def interruptibleBlockingEffect(cancelledFlag: AtomicBoolean): Task[Unit] = {
    ZIO.attemptBlockingCancelable { //effect
      (1 to 100000).foreach { element =>
        if (!cancelledFlag.get()) {
          println(element)
          Thread.sleep(100)
        }
      }
    } (ZIO.succeed(cancelledFlag.set(true))) //cancelling effect
  }

  val interruptableBlockingDemo = for {
    fib <- interruptibleBlockingEffect(new AtomicBoolean(false)).fork
    _ <- ZIO.sleep(2.seconds) *> ZIO.succeed("interrupting...").debugThread *> fib.interrupt
    _ <- fib.join
  } yield ()

  //Semantic blocking


  def run = interruptableBlockingDemo
}
