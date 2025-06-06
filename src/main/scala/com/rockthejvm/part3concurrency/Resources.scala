package com.rockthejvm.part3concurrency

import com.rockthejvm.utils.*
import zio.*

import java.io.File
import java.util.Scanner

object Resources extends ZIOAppDefault {

  //finalizers ensures completion of effect

  def unsafeMethod(): Int = throw new RuntimeException("Not an int here!")

  val anAttempt = ZIO.attempt(unsafeMethod())

  //finalizers
  val attemptWithFinalizer = anAttempt.ensuring(ZIO.succeed("finalizer!").debugThread) //will succeed before attempt fails

  //multiple finalizers
  val attemptWith2Finalizers = attemptWithFinalizer.ensuring(ZIO.succeed("another finalizer!").debugThread) //both will succeed before attempt fails

  //other finalizers:
  // onInterrupt, onError, onDone, onExit

  //resource lifecycle
  class Connection(url: String) {
    def open() = ZIO.succeed(s"opening connection to $url...").debugThread

    def close() = ZIO.succeed(s"closing connection to $url...").debugThread
  }

  object Connection {
    def create(url: String) = ZIO.succeed(new Connection(url))
  }

  //this will cause resource leak:
  val fetchUrl = for {
    conn <- Connection.create("rockthejvm.com")
    fib <- (conn.open() *> ZIO.sleep(300.seconds)).fork
    _ <- ZIO.sleep(1.second) *> ZIO.succeed("interrupting").debugThread *> fib.interrupt
    _ <- fib.join
  } yield ()

  //with finalizers, it will not leak - but it gets tedious as the code gets complex
  val correctFetchUrl = for {
    conn <- Connection.create("rockthejvm.com")
    fib <- (conn.open() *> ZIO.sleep(300.seconds)).fork
    _ <- ZIO.sleep(1.second) *> ZIO.succeed("interrupting").ensuring(conn.close()).fork *> fib.interrupt
    _ <- fib.join
  } yield ()

  // acquireRelease does the closing for us!
  // properites:
  // - acquiring cannot be interrupted
  // - all finalizers are guaranteed to run
  // - has a scope dependency
  val cleanConnection = ZIO.acquireRelease(Connection.create("rockthejvm.com"))(_.close())

  val fetchWithResource = for {
    conn <- cleanConnection
    fib <- (conn.open() *> ZIO.sleep(300.seconds)).fork
    _ <- ZIO.sleep(1.second) *> ZIO.succeed("interrupting").debugThread *> fib.interrupt
    _ <- fib.join
  } yield ()

  val fetchWithScopedResource = ZIO.scoped(fetchWithResource)

  // acquireReleaseWith
  val cleanConnection_v2 = ZIO.acquireReleaseWith(
    Connection.create("rockthejvm.com") // acquire
  )(
    _.close() // release
  )(conn =>
    conn.open() *> ZIO.sleep(300.seconds) // use
  )

  val fetchWithResource_v2 = for {
    fib <- cleanConnection_v2.fork
    _ <- ZIO.sleep(1.second) *> ZIO.succeed("interrupting").debugThread *> fib.interrupt
    _ <- fib.join
  } yield ()

  //Exercise
  //1

  def acquireOpenFile(path: String): ZIO[Any, Nothing, Unit] = {
    ZIO.succeed(s"opening file at $path").debugThread *>
      ZIO.acquireReleaseWith(
        openFileScanner(path) // acquire
      )(
        scanner => ZIO.succeed(s"closing file at $path").debugThread *> ZIO.succeed(scanner.close())
      )(
        scanner => readLineByLine(scanner) // use
      )
  }

  def openFileScanner(path: String): UIO[Scanner] = {
    ZIO.succeed(new Scanner(new File(path)))
  }

  def readLineByLine(scanner: Scanner): UIO[Unit] = {
    if (scanner.hasNextLine) {
      ZIO.succeed(scanner.nextLine()).debugThread *> ZIO.sleep(100.millis) *> readLineByLine(scanner)
    } else ZIO.unit
  }

  val testInterruptFileDisplay = for {
    fib <- acquireOpenFile("src/main/scala/com/rockthejvm/part3concurrency/Resources.scala").fork
    _ <- ZIO.sleep(2.seconds) *> fib.interrupt
  } yield ()

  //acquirerelease vs with :

  def connFromConfig(path: String): UIO[Unit] = {
    ZIO.acquireReleaseWith(openFileScanner(path))(scanner => ZIO.succeed("closing file").debugThread *> ZIO.succeed(scanner.close())) { scanner =>
      ZIO.acquireReleaseWith(Connection.create(scanner.nextLine()))(_.close()) { conn =>
        conn.open() *> ZIO.never
      }
    }
  } //HARD TO READ, DO THIS INSTEAD WITH NESTED RESOURCES:

  def connFromConfig_v2(path: String): UIO[Unit] = ZIO.scoped {
    for {
      scanner <- ZIO.acquireRelease(openFileScanner(path))(scanner => ZIO.succeed("closing file").debugThread *> ZIO.succeed(scanner.close()))
      conn <- ZIO.acquireRelease(Connection.create(scanner.nextLine()))(_.close())
      _ <- conn.open() *> ZIO.never
    } yield ()
  }

  def run = connFromConfig_v2("src/main/resources/connection.conf")
}
