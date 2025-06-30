package com.rockthejvm.part4coordination

import zio.*
import com.rockthejvm.utils.*

import java.util.concurrent.TimeUnit
object Refs extends ZIOAppDefault {

  //refs are purely functional wrappers around atomic references
  val atomicMOL: ZIO[Any, Nothing, Ref[Int]] = Ref.make(42)

  //obtaining a value
  val mol = atomicMOL.flatMap {ref =>
    ref.get //returns a UIO[Int], thread safe getter
  }

  //changing
  val setMol = atomicMOL.flatMap {ref =>
    ref.set(100) //UIO[Unit], thread-safe setter
  }

  // get + change combined operation
  val gsMol = atomicMOL.flatMap {ref =>
    ref.getAndSet(500)
  }

  // update - run a function on the value (note: will try again if the ref is in use by some other fiber)
  val updateMol: UIO[Unit] = atomicMOL.flatMap{ref =>
    ref.update(_ * 100)
  }

  // update + get in one operation - runs on values of the same type
  val updatedMolWithValue = atomicMOL.flatMap{ref =>
    ref.getAndUpdate(_ * 100) //returns the NEW value
    ref.updateAndGet(_ * 100) //returns the OLD value
  }

  //modify - returns tuple with modified current value and new value (note: will try again if the ref is in use by some other fiber)
  val modifiedMol: UIO[String] = atomicMOL.flatMap{ref =>
    ref.modify(value => (s"my current value is $value", value * 100))
  }

  //example distributing work- this is not thread safe, mixing pure and impure code:
  def demoConcurrentWorkImpure(): UIO[Unit] = {
    var count = 0

    //do the task, and update variable with new count
    def task(workload: String): UIO[Unit] = {
      val wordCount = workload.split(" ").length
      for {
        _ <- ZIO.succeed(s"counting words for: $workload: $wordCount").debugThread
        newCount <- ZIO.succeed(count + wordCount)
        _ <- ZIO.succeed(s"new total: $newCount").debugThread
        _ <- ZIO.succeed(count += wordCount)
      } yield ()
    }

    val effects = List("I love ZIO", "This Ref thing is cool", "Daniel writes a lot of code").map(task)
    ZIO.collectAllParDiscard(effects)
  }

  def demoConcurrentWorkPure(): UIO[Unit] = {

    def task(workload: String, total: Ref[Int]): UIO[Unit] = {
      val wordCount = workload.split(" ").length
      for {
        _ <- ZIO.succeed(s"counting words for: $workload: $wordCount").debugThread
        newCount <- total.updateAndGet(_ + wordCount)
        _ <- ZIO.succeed(s"new total: $newCount").debugThread
      } yield ()
    }

    for {
      counter <- Ref.make(0)
      _ <- ZIO.collectAllParDiscard(
        List("I love ZIO", "This Ref thing is cool", "Daniel writes a lot of code")
          .map(load => task(load, counter))
      )
    } yield ()
  }

  //Exercise

//  def tickingClockImpure(): Unit = {
//    val ticks = 0L
//
//    // print the current time every 1 second and increase counter
//    def tickingClock: UIO[Unit] = for {
//      _ <- ZIO.sleep(1.second)
//      _ <- Clock.currentTime(TimeUnit.MILLISECONDS).debugThread
//      _ <- ZIO.succeed(ticks + 1)
//      _ <- tickingClock
//    } yield ()
//
//    // print the total ticks count every 5 seconds
//    def printTicks: UIO[Unit] = for {
//      _ <- ZIO.sleep(5.seconds)
//      _ <- ZIO.succeed(s"ticks: $ticks").debugThread
//      _ <- printTicks
//    } yield ()
//
//    (tickingClock.zipPar(printTicks)).unit
//  }

  def tickingClockPure(): UIO[Unit] = {
    // print the current time every 1 second and increase counter
    def tickingClock(ticks: Ref[Long]): UIO[Unit] = for {
      _ <- ZIO.sleep(1.second)
      _ <- Clock.currentTime(TimeUnit.MILLISECONDS).debugThread
      _ <- ticks.updateAndGet(_ + 1)
      _ <- tickingClock(ticks)
    } yield ()

    // print the total ticks count every 5 seconds
    def printTicks(ticks: Ref[Long]): UIO[Unit] = for {
      _ <- ZIO.sleep(5.seconds)
      count <- ticks.get
      _ <- ZIO.succeed(s"ticks: $count").debugThread
      _ <- printTicks(ticks)
    } yield ()

    Ref.make(0L).flatMap{ref =>
      (tickingClock(ref).zipPar(printTicks(ref))).unit
    }
  }
  def run = tickingClockPure()
}
