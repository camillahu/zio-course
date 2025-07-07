package com.rockthejvm.part4coordination

import zio._
import com.rockthejvm.utils._

object Promises extends ZIOAppDefault  {

  val aPromise:ZIO[Any, Nothing, Promise[Throwable, Int]] = Promise.make[Throwable, Int]

  //await - block the fiber until the promise has a value
  val reader = aPromise.flatMap {promise =>
    promise.await
  }

  //succeed, fail, complete
  val writer = aPromise.flatMap { promise =>
    promise.succeed(42)
  }

  def demoPromise(): Task[Unit] = {
    //producer - consumer problem
    def consumer(promise: Promise[Throwable, Int]): Task[Unit] = for {
      _ <- ZIO.succeed("[consumer] waiting for result...").debugThread
      mol <- promise.await
      _ <- ZIO.succeed(s"[consumer] I got the result: $mol").debugThread
    } yield ()

    def producer(promise: Promise[Throwable, Int]): Task[Unit] = for {
      - <- ZIO.succeed("[producer] crunching numbers...").debugThread
      _ <- ZIO.sleep(3.seconds)
      - <- ZIO.succeed("[producer] complete.").debugThread
      mol <- ZIO.succeed(42)
      _ <- promise.succeed(mol)
    } yield ()

    for{
      promise <- Promise.make[Throwable, Int]
      _ <- consumer(promise) zipPar producer(promise)
    } yield()
  }

  /*
  * purely functional block on a fiber until you get a signal from another fiber
  * waiting on a value which may not yet be available, without thread starvation
  * inter-fiber communication
  *  */

  //simulate downloading from multiple parts- difference between ref and promise
  val fileParts= List("I ", "love S", "cala", " with pure FP an", "d ZIO! <EOF>")
  def downloadFileWithRef(): Task[Unit] = {
    def downloadFile(contentRef: Ref[String]): Task[Unit] = {
      ZIO.collectAllDiscard(
        fileParts.map{part =>
          ZIO.succeed(s"got '$part'").debugThread *> ZIO.sleep(1.second) *> contentRef.update(_ + part)
        }
      )
    }

    //will need to "busy wait" with a ref. Uses more resources
    def notifyFileComplete(contentRef: Ref[String]): Task[Unit] = for {
      file <- contentRef.get
      _ <-  if(file.endsWith("<EOF>")) ZIO.succeed("File download complete...").debugThread
      else ZIO.succeed("downloading...").debugThread *> ZIO.sleep(500.millis) *> notifyFileComplete(contentRef)
    } yield ()

    for{
      contentRef <- Ref.make("")
      _ <- downloadFile(contentRef) zipPar notifyFileComplete(contentRef)
    } yield ()
  }

  def downloadFileWithRefAndPromise(): Task[Unit] = {
      def downloadFile(contentRef: Ref[String], promise: Promise[Throwable, String]): Task[Unit] =
        ZIO.collectAllDiscard(
          fileParts.map { part =>
            for {
              _ <- ZIO.succeed(s"got '$part'").debugThread
              _ <- ZIO.sleep(1.second)
              file <- contentRef.updateAndGet(_ + part)
              _ <- promise.succeed(file).when(file.endsWith("<EOF>"))
            } yield ()
          }
        )


      //no need to "busy wait" with a ref and promise.
      def notifyFileComplete(contentRef: Ref[String], promise: Promise[Throwable, String]): Task[Unit] = for {
        _ <- ZIO.succeed("downloading...").debugThread
        file <- promise.await
        _ <- ZIO.succeed(s"file download complete '$file'").debugThread
      } yield ()

      for {
        contentRef <- Ref.make("")
        promise <- Promise.make[Throwable, String]
        _ <- downloadFile(contentRef, promise) zipPar notifyFileComplete(contentRef, promise)
      } yield ()
  }

  def run = downloadFileWithRefAndPromise()
}
