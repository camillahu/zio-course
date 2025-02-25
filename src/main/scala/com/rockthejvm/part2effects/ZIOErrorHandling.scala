package com.rockthejvm.part2effects

import zio.*

import java.io.IOException
import java.net.NoRouteToHostException
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

  /*
     ERRORS VS DEFECTS
      Errors: failures present in the ZIO type signature ("checked" errors)
      Defects: failures that are unrecoverable, unforeseen, NOT present in the ZIO type signature

      ZIO[R,E,A] can finish with Exit[E, A]
        Success[A] containig a value
        Cause[E]
          Fail[E] containing an error
          Die(t: Throwable) which was unforeseen
   */

  val divisionByZero: UIO[Int] = ZIO.succeed(1 / 0)

  val failedInt: ZIO[Any, String, Int] = ZIO.fail("I failed!")
  val failureCauseExposed: ZIO[Any, Cause[String], Int] = failedInt.sandbox
  val failureCauseHidden: ZIO[Any, String, Int] = failureCauseExposed.unsandbox
  //fold with cause
  val foldedWithCause = failedInt.foldCause(
    cause => ZIO.succeed(s"this failed with ${cause.defects}"),
    value => ZIO.succeed(s"this succeed with $value")
  )

  val foldWithCause_v2 = failedInt.foldCauseZIO(
    cause => ZIO.succeed(s"this failed with ${cause.defects}"),
    value => ZIO.succeed(s"this succeeded with $value")
  )

  //Good practice:
  //  - at a lower level, your "errors" should be treated
  //  - at a higher level, you should hide "errors" and assume they are unrecoverable

  def callHTTPEndpoint(url: String): ZIO[Any, IOException, String] = {
    ZIO.fail(new IOException("no internet, dummy!"))
  }

  val endpointCallWithDefects: ZIO[Any, Nothing, String] = {
    callHTTPEndpoint("rockthejvm.com").orDie // all errors are now defects -- swallow IOException
  }

  //refining the error channel -- error channel is a big superclass, method can be used to refine later on (see below).
  def callHTTPEndpointWideError(url: String): ZIO[Any, Exception, String] = {
    ZIO.fail(new IOException("No internet!!"))
  }

  //-- some errors "allowed", rest are defects
  def callHTTPEndpoint_v2(url: String): ZIO[Any, IOException, String] = {
    callHTTPEndpointWideError(url).refineOrDie[IOException] {
      case e: IOException => e
      case _: NoRouteToHostException => new IOException(s"No route to host to $url, can't fetch page")
    }
  }

  //reverse: turn defects into the error channel
  val endpointCallWithError = endpointCallWithDefects.unrefine {
    case e => e.getMessage
  }


  //Combine effects with different errors

//  This one loses type safety and is NOT good practice:
  //  case class IndexError(message: String)
  //  case class DbError(message: String)
  //  val callAPI: ZIO[Any, IndexError, String] = ZIO.succeed("page: <html></html>")
  //  val queryDb: ZIO[Any, DbError, Int] = ZIO.succeed(1)
  //  val combined = for {
  //    page <- callAPI
  //    rowsAffected <- queryDb
  //  } yield (page, rowsAffected)

  //SOLUTIONS:

  //Design error model(best solution):
  trait AppError
  case class IndexError(message: String) extends AppError

  case class DbError(message: String) extends AppError

  val callAPI: ZIO[Any, IndexError, String] = ZIO.succeed("page: <html></html>")
  val queryDb: ZIO[Any, DbError, Int] = ZIO.succeed(1)
  val combined = for {
    page <- callAPI
    rowsAffected <- queryDb
  } yield (page, rowsAffected)

  //Scala 3 union types (not available for Scala 2):
  // val combined_v2: ZIO[Any, IndexError | DbError, (String, Int)] = ??? // a for comp or pattern match

  // .mapError to some common error type

  //EXERCISES

  //1
  val aBadFailure = ZIO.succeed[Int](throw new RuntimeException("this is bad!"))
  def aBetterFailure(failedZIO: UIO[Int]): ZIO[Any, Cause[String], Int] = failedZIO.sandbox

  //2
  def ioException[R,A](zio: ZIO[R, Throwable, A]): ZIO[R, IOException, A] = {
    zio.refineOrDie[IOException] {
      case e: IOException => e
      case _: Exception => new IOException(s"unidentified error")
    }
  }

  //4
  val database = Map(
    "daniel" -> 123,
    "alice" -> 789
  )

  trait QueryError
  case class IDFormatError(reason: String) extends QueryError
  case class UserNotFoundError(reason: String) extends QueryError

  case class UserProfile(name: String, phone: Int)

//  def lookupProfile(userId: String): ZIO[Any, QueryError, Option[UserProfile]] = {
//    if (userId != userId.toLowerCase())
//      ZIO.fail(IDFormatError("user ID format is invalid"))
//    else
//      ZIO.succeed(database.get(userId).map(phone => UserProfile(userId, phone)))
//  }

  //surface out all the failed cases of this API
  def betterLookupProfile(userId: String):  ZIO[Any, QueryError, UserProfile] = {
    if (userId != userId.toLowerCase()) {
        ZIO.fail(IDFormatError(s"username format '$userId' is invalid'"))
    }
    else {
        database.get(userId) match {
          case Some(phone) => ZIO.succeed(UserProfile(userId, phone))
          case None => ZIO.fail(IDFormatError(s"user '$userId' not found'"))
        }
    }
  }


  //SOLUTION

  //1
  val aBetterFailure = aBadFailure.sandbox // exposes the defect in the Cause
  val aBetterFailure_v2 = aBadFailure.unrefine { // surfaces out the exception in the error channel
    case e => e
  }

  //2
  def ioException_v2[R, A](zio: ZIO[R, Throwable, A]): ZIO[R, IOException, A] = {
    zio.refineOrDie {
      case e: IOException => e //everything else is a defect
    }
  }


  //3
  def left[R,E,A,B](zio: ZIO[R,E,Either[A,B]]): ZIO[R, Either[E,A], B] = {
    zio.foldZIO(
      e => ZIO.fail(Left(e)),
      either => either match {
        case Left(a) => ZIO.fail(Right(a))
        case Right(b) => ZIO.succeed(b)
      }
    )
  }

  //4

  case class QueryError_v2(reason: String)

  def lookupProfile(userId: String): ZIO[Any, QueryError_v2, Option[UserProfile]] = {
    if (userId != userId.toLowerCase())
      ZIO.fail(QueryError_v2("user ID format is invalid"))
    else
      ZIO.succeed(database.get(userId).map(phone => UserProfile(userId, phone)))
  }

  def betterLookupProfile_v2(userId: String): ZIO[Any, Option[QueryError_v2], UserProfile] = {
    lookupProfile(userId).foldZIO(
      error => ZIO.fail(Some(error)),
      profileOption => profileOption match {
        case Some(user) => ZIO.succeed(UserProfile(user.name, user.phone))
        case None => ZIO.fail(None)
      }
    )
  }

  //some does the same as the above
  def betterLookupProfile_v3(userId: String): ZIO[Any, Option[QueryError_v2], UserProfile] =
    lookupProfile(userId).some



  override def run = {
    aBetterFailure(aBadFailure).debug


  }
}
