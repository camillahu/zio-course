package com.rockthejvm.part3concurrency
import zio._
import com.rockthejvm.utils._

object MasteringInterruptions extends ZIOAppDefault {

  //interruptions:
  // fib.interrupt
  // ZIO.race, ZIO.zipPar, ZIO.collectAllPar
  // outliving parent fiber

  //manual interruption: (42 won't happen)
  val aManuallyInterruptedZIO = ZIO.succeed("computing...").debugThread *> ZIO.interrupt *> ZIO.succeed(42).debugThread

  // finalizer: (makes sure 42 finishes even though it is interrupted)
  val effectWithInterruptionFinalizer = aManuallyInterruptedZIO.onInterrupt(ZIO.succeed("I was interrupted!").debugThread)

  // uninterruptible
  val fussyPaymentSystem = (
    ZIO.succeed("payment running, don't cancel me...").debugThread *>
      ZIO.sleep(1.second) *> // the actual payment
    ZIO.succeed("payment completed").debugThread
  ).onInterrupt(ZIO.succeed("MEGA CANCEL OF DOOM!").debugThread) //don't want this to be triggered

  val cancellationOfDoom = for {
    fib <- fussyPaymentSystem.fork
    _ <- ZIO.sleep(500.millis) *> fib.interrupt
    _ <- fib.join
  } yield ()

  //ZIO.uninterruptable
  val atomicPayment = ZIO.uninterruptible(fussyPaymentSystem) //make a ZIO atomic
  val atomicPayment_v2 = fussyPaymentSystem.uninterruptible //same

  val noCancellationOfDoom = for {
    fib <- atomicPayment.fork
    _ <- ZIO.sleep(500.millis) *> fib.interrupt
    _ <- fib.join
  } yield ()

  //interruptibility is regional
  val zio1 = ZIO.succeed(1)
  val zio2 = ZIO.succeed(2)
  val zio3 = ZIO.succeed(3)
  val zioComposed = (zio1 *> zio2 *> zio3).uninterruptible //all zios are uninterruptible
  val zioComposed2 = (zio1 *> zio2.interruptible *> zio3).uninterruptible //inner scopes override outer scopes

  // uninterruptibleMask - makes all effects wrapped inside interruptible

  /*
  * Example: an auth service
  - input password, can be interrupted, because otherwise it might block the fiber indefinitely
  - verfiy password, cannot be interrupted once triggered
  * */

  val inputPassword = for {
    _ <- ZIO.succeed("Input password:").debugThread
    _ <- ZIO.succeed("(typing password)").debugThread
    _ <- ZIO.sleep(2.seconds)
    pass <- ZIO.succeed("RockTheJVM1!")
  } yield pass

  def verifyPassword(pw: String) = for {
    _ <- ZIO.succeed("verifying...").debugThread
    _ <- ZIO.sleep(2.seconds)
    result <- ZIO.succeed(pw == "RockTheJVM1!")
  } yield result

  val authFlow = ZIO.uninterruptibleMask { restore =>
    //everything is uninterruptible except for restore! you can invoke restore multiple times if needed
   for {
     pw <- restore(inputPassword).onInterrupt(ZIO.succeed("Auth timed out. Try again later.").debugThread)
     verification <- verifyPassword(pw)
     _ <- if(verification) ZIO.succeed("Auth successful").debugThread
     else ZIO.succeed("Auth failed").debugThread
   } yield ()
  }

  val authProgram = for {
    authFib <- authFlow.fork
    _ <- ZIO.sleep(3.seconds) *> ZIO.succeed("Attempting to cancel auth...").debugThread *> authFib.interrupt
    _ <- authFib.join
  } yield ()  //will not interrupt before after authFlow is completed!

  def run = authProgram
}
