package com.rockthejvm.part2effects

import zio._

import scala.util.{Failure, Success, Try}

object ZIOErrorHandling extends ZIOAppDefault {

  //ZIOs can fail
  val aFailedZIO = ZIO.fail("Something went wrong")
  val failedWithThrowable = ZIO.fail(new RuntimeException("Boom!"))

  val badZIO = ZIO.succeed {
    println("Trying something")
    val string: String = null
    string.length
  } // this is bad, using ZIO succeed you need to be 100% sure that the code will not fail


  // use attempt if you're ever unsure whether your code might throw
  val anAttempt = ZIO.attempt {
    println("Trying something")
    val string: String = null
    string.length
  }

  // effectfullt catch errors
  val catchError = anAttempt.catchAll(e => ZIO.succeed(s"Returning a different value because $e"))
  val catchSelectiveErrors = anAttempt.catchSome {
    case e: RuntimeException => ZIO.succeed(s"Ignoring runtime exceptions $e")
    case _ => ZIO.succeed("Ignoring everything else")
  }

  // chain effects
  val aBetterAttempt = anAttempt.orElse(ZIO.succeed(56))
  // fold: handle both success and failure
  val handleBoth: ZIO[Any, Nothing, String] = anAttempt.fold(ex => s"Something bad happened: $ex", value => s"Length of string was $value")
  // effectful fold: foldZIO
  val handleBoth_v2 = anAttempt.foldZIO(
    ex => ZIO.succeed(s"Something bad happened: $ex"),
    value => ZIO.succeed(s"Length of string was $value")
  )

  // CONVERSIONS BETWEEN OPTION/TRY/EITHER to ZIO

  val aTryToZIO: ZIO[Any, Throwable, Int] = ZIO.fromTry(Try(42/ 0)) //can fail with throwable

  //either -> ZIO
  val anEither: Either[Int, String] = Right("Success")
  val anEitherToZIO: ZIO[Any, Int, String] = ZIO.fromEither(anEither)
  // ZIO -> ZIO with Either as the value channel
  val eitherZIO = anAttempt.either
  //reverse -- Either with ZIO as output
  val anAttempt_v2 = eitherZIO.absolve

  //option -> ZIO
  val anOption: ZIO[Any, Option[Nothing], Int] = ZIO.fromOption(Some(42))

  //exercise

  val anEitherAttempt = anEitherToZIO.foldZIO(
    ex => ZIO.succeed(s"Something went wrong $ex"),
    value => ZIO.succeed(s"ZIO successful with value: $value")
  )
  

  val aTryAttempt = aTryToZIO.foldZIO(
    ex => ZIO.succeed(s"Something went wrong $ex"),
    value => ZIO.succeed(s"ZIO successful with value: $value")
  )

  val anOptionAttempt = anOption.foldZIO (
    ex => ZIO.succeed(s"Something went wrong $ex"),
    value => ZIO.succeed(s"ZIO successful with value: $value")
  )

  //SOLUTIONS

  def try2ZOO[A](aTry: Try[A]): Task[A] = aTry match {
    case Failure(exception) => ZIO.fail(exception)
    case Success(value) => ZIO.succeed(value)
  }

  def either2ZIO[A, B](anEither: Either[A, B]): ZIO[Any, A, B] = anEither match {
    case Left(value) => ZIO.fail(value)
    case Right(value) => ZIO.succeed(value)
  }

  def option2ZIO[A](anOption: Option[A]): ZIO[Any, Option[Nothing], A] = anOption match {
    case Some(value) => ZIO.succeed(value)
    case None => ZIO.fail(None)
  }

  def zio2zioEither[R,A,B](zio: ZIO[R,A,B]): ZIO[R, Nothing, Either[A,B]] = zio.foldZIO(
    error => ZIO.succeed(Left(error)),
    value => ZIO.succeed(Right(value))
  )

  def absolveZIO[R,A,B](zio: ZIO[R, Nothing, Either[A, B]]): ZIO[R,A,B] = zio.flatMap {
    case Left(e) => ZIO.fail(e)
    case Right(v)=> ZIO.succeed(v)
  }

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = ???
  
}
