package com.rockthejvm.part4coordination
import zio._
import com.rockthejvm.utils._

object Semaphores extends ZIOAppDefault {
  // synchronization primitive that allows only a certain number of threads in a critical section.
  // more general version of mutex.

  // a semaphore has:
  // n permits that it can allocate to fibers
  // acquire, acquireN (purely functional, can potentially semantically block until semaphore has enough permits)
  // release, releaseN

  // make a semaphopre:
  val aSemaphore = Semaphore.make(10)
  // Semaphore.make(1) == a Mutex

  // use case example: limiting the number of concurrent sessions on a server
  def doWhileLoggedIn(): UIO[Int] =
    ZIO.sleep(1.second) *> Random.nextIntBounded(100)

  def login(id: Int, sem: Semaphore): UIO[Int] = {
    ZIO.succeed(s"[task $id] waiting to log in").debugThread *>
      sem.withPermit { // acquire + zio + release (you can also use withPermits which is acquire with multiple permits at the same time)
        for {
          // critical section start - will only be able to start if it has enough(at least one) permits when the ZIO is executed.
          _ <- ZIO.succeed(s"[task $id] logged in, working...").debugThread
          res <- doWhileLoggedIn()
          _ <- ZIO.succeed(s"[task $id] done: $res").debugThread
        } yield res
      }
  }

  def demoSemaphore() = for {
    sem <- Semaphore.make(2)
    f1 <- login(1, sem).fork
    f2 <- login(2, sem).fork
    f3 <- login(3, sem).fork
    _ <- f1.join
    _ <- f2.join
    _ <- f3.join
  } yield ()

  def loginWeighted(n: Int, sem: Semaphore): UIO[Int] = {
    ZIO.succeed(s"[task $n] waiting to log in with $n permits").debugThread *>
      sem.withPermits(n) { // acquire multiple permits + zio + release
        for {
          // critical section starts when you acquired ALL n permits.
          _ <- ZIO.succeed(s"[task $n] logged in, working...").debugThread
          res <- doWhileLoggedIn()
          _ <- ZIO.succeed(s"[task $n] done: $res").debugThread
        } yield res
      }
  }

  def demoSemaphoreWeighted() = for {
    sem <- Semaphore.make(2)
    f1 <- loginWeighted(1, sem).fork // requires 1 permit
    f2 <- loginWeighted(2, sem).fork // requires 2 permits
    f3 <- loginWeighted(3, sem).fork // requires 3 permits - will block indefinitely because we only have 2 fibers available.
    _ <- f1.join
    _ <- f2.join
    _ <- f3.join
  } yield ()

  //exercise
  val mySemaphore = Semaphore.make(1)
  val tasks = ZIO.collectAllPar((1 to 10).map { id =>
    for {
      sem <- mySemaphore
      _ <- ZIO.succeed(s"[task $id] waiting to log in").debugThread
      res <- sem.withPermit {
        for {
          // critical section start
          _ <- ZIO.succeed(s"[task $id] logged in, working...").debugThread
          res <- doWhileLoggedIn()
          _ <- ZIO.succeed(s"[task $id] done: $res").debugThread
        } yield res
      }
    } yield res
  })

  val tasksFixed = mySemaphore.flatMap { sem =>
    ZIO.collectAllPar((1 to 10).map { id =>
      for {
        _ <- ZIO.succeed(s"[task $id] waiting to log in").debugThread
        res <- sem.withPermit {
          for {
            // critical section start
            _ <- ZIO.succeed(s"[task $id] logged in, working...").debugThread
            res <- doWhileLoggedIn()
            _ <- ZIO.succeed(s"[task $id] done: $res").debugThread
          } yield res
        }
      } yield res
    })
  }

  override def run = tasksFixed.debugThread
}
