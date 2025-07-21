package com.rockthejvm.part4coordination

import zio._
import zio.stm._
import com.rockthejvm.utils._

object TransactionalEffects extends ZIOAppDefault {

  // STM - Software transactional memory "atomic effects"
  // atomic effect = once an effect starts it can not be interrupted and will fully complete.

  val anSTM: ZSTM[Any, Nothing, Int] = STM.succeed(42)
  val aFailedSTM = STM.fail("something bad")
  val anAttemptSTM: ZSTM[Any, Throwable, Int] = STM.attempt(42 / 0)

  //map, flatMap, for comprehensions

  // type aliases - the two below are most used in practise, but there are more.
  val ustm: USTM[Int] = STM.succeed(2) //comparable to UIO
  val anSTM_v2: STM[Nothing, Int] = STM.succeed(42) //comparable to IO

  //STM vs ZIO
  // you can compose STMs to obtain other STMs
  // evaluation is fully atomic (cannot be run unsafely in the same way as a ZIO)
  // will block all shared resources when evaluated
  // it is a description of a computation, not the actual computation.
  // when you evaluate the description, you evaluate to a ZIO instead. This is "commit" (TURNING SMT TO ZIO)

  val anAtomicEffect: ZIO[Any, Throwable, Int] = anAttemptSTM.commit

  //example when NOT blocking shared resources:
  // (this can result in receiver getting double the amount even though sender doesn't have any more funds)
  def transferMoney(sender: Ref[Long], receiver: Ref[Long], amount: Long): ZIO[Any, String, Long] = {
    for {
      senderBalance <- sender.get
      _ <- ZIO.fail("Transfer failed: Insufficient funds.").when(senderBalance < amount)
      _ <- sender.update(_ - amount)
      _ <- receiver.update(_ + amount)
      newBalance <- sender.get
    } yield newBalance
  }

  def exploitBuggyBank() = for {
    sender <- Ref.make(1000L)
    receiver <- Ref.make(0L)
    fib1 <- transferMoney(sender, receiver, 1000).fork
    fib2 <- transferMoney(sender, receiver, 1000).fork
    _ <- (fib1 zip fib2).join
    _ <- receiver.get.debugThread
  } yield ()

  def loop(effect: ZIO[Any, String, Unit], i: Int): ZIO[Any, Nothing, Unit] = {
    if (i > 10000)
      ZIO.unit
    else effect.ignore *> loop(effect, i + 1)
  }

  //STM implementation: TRef is a transactional ref- it "belongs" to SMT.
  def transferMoneyTransactional(sender: TRef[Long], receiver: TRef[Long], amount: Long): STM[String, Long] = {
    for {
      senderBalance <- sender.get
      _ <- STM.fail("Transfer failed: Insufficient funds.").when(senderBalance < amount)
      _ <- sender.update(_ - amount)
      _ <- receiver.update(_ + amount)
      newBalance <- sender.get
    } yield newBalance
  }

  def cannotExploit() = for {
    sender <- TRef.make(1000L).commit
    receiver <- TRef.make(0L).commit
    fib1 <- transferMoneyTransactional(sender, receiver, 1000).commit.fork
    fib2 <- transferMoneyTransactional(sender, receiver, 1000).commit.fork
    _ <- (fib1 zip fib2).join
    _ <- receiver.get.commit.debugThread
  } yield ()
  //since it is atomic, only one of the fibers will complete commit.

  def run = loop(cannotExploit(), 1)

}
