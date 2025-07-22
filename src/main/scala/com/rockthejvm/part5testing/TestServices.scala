package com.rockthejvm.part5testing

import zio.test._
import zio._
import zio.test.TestAspect._

object SimpleDependencySpec extends ZIOSpecDefault {
  def spec = test("simple dependency") {
    //provide dependencies
    val aZIO: ZIO[Int, Nothing, Int] = ZIO.succeed(42)
    assertZIO(aZIO.provide(ZLayer.succeed(10)))(Assertion.equalTo(42))

    //provide dependencies at the end
    val aZIO_v2: ZIO[Int, Nothing, Int] = ZIO.succeed(42)
    assertZIO(aZIO)(Assertion.equalTo(42))
  }.provide(ZLayer.succeed(10))
}

// example: a user survey application processing user data, fetching data from a db
object BusinessLogicSpec extends ZIOSpecDefault {

  // "dependency"
  abstract class Database[K, V] {
    def get(key: K): Task[V]
    def put(key: K, value: V): Task[Unit]
  }
  object Database {
    def create(url: String): UIO[Database[String, String]] = ???
  }

  //"business" logic under test
  def normalizeUsername(name: String): UIO[String] =
    ZIO.succeed(name.toUpperCase())

  val mockedDatabase = ZIO.succeed(new Database[String, String] {
    import scala.collection.mutable
    val map = mutable.Map[String, String]()

    override def get(key: String) = ZIO.attempt(map(key))
    override def put(key: String, value: String) = ZIO.attempt(map += (key -> value))
  })

  // testing
  def spec = suite("A user surveu application should...")(
    test("normalize user names") {
      val surveyPreliminaryLogic = for {
        db <- ZIO.service[Database[String, String]]
        _ <- db.put("123", "Daniel")
        username <- db.get("123")
        normalized <- normalizeUsername(username)
      } yield normalized
      assertZIO(surveyPreliminaryLogic)(Assertion.equalTo("DANIEL"))
    }
  ).provide(ZLayer.fromZIO(mockedDatabase))
}

 /*
  built-in test services
  - console
  - random
  - clock
  - system
*/

object DummyConsoleApplication {
  def welcomeUser(): Task[Unit] = for {
    _ <- Console.printLine("Please enter your name...")
    name <- Console.readLine("")
    _ <- Console.printLine(s"Welcome, $name!")
  } yield ()
}

object BuiltInTestServiceSpec extends ZIOSpecDefault {
  def spec = suite("Checking built-in test services")(
    test("ZIO console application") {
      val logicUnderTest: Task[Vector[String]] = for {
        //will feed the lines to the dummy application as it was in the console.
        _ <- TestConsole.feedLines("Daniel")
        _ <- DummyConsoleApplication.welcomeUser()
        output <- TestConsole.output
      } yield output.map(_.trim)

      assertZIO(logicUnderTest)(Assertion.hasSameElements(
        List("Please enter your name...", "", "Welcome, Daniel!")))
    },
    test("ZIO clock") {
      //used to manipulate the passage of time to test how long something takes.
      val parallelEffect = for {
        fiber <- ZIO.sleep(5.minutes).timeout(1.minute).fork
        _ <- TestClock.adjust(1.minute)
        result <- fiber.join
      } yield result

      assertZIO(parallelEffect)(Assertion.isNone)
    },
    test("ZIO Random") {
      val effect = for {
        _ <- TestRandom.feedInts(3,4,1,2)
        value <- Random.nextInt
      } yield value

      assertZIO(effect)(Assertion.equalTo(3))
    }
  )
}

/*
  Test aspects
*/

object AspectsSpec extends ZIOSpecDefault {
  def computeMeaningOfLife: UIO[Int] = {
    ZIO.sleep(2.seconds) *> ZIO.succeed(42)
  }

  def spec = suite("Testing Ascpects")(
    test("timeout aspect") {
      val effect = for {
        molFib <- computeMeaningOfLife.fork
        _ <- TestClock.adjust(3.seconds)
        v <- molFib.join
      } yield v

      assertZIO(effect)(Assertion.equalTo(42))
    } @@ timeout(10.seconds) @@ diagnose(1.second)

    /*
      Aspects:
      - timeout(duration)
      - eventually - retries until successful
      - nonFlaky(n) repeats n times, stopes at first failure
      - repeats(n) - same
      - retries(n) - retries n times, stops at first success
      - debug - prints everything in the console
      - silent - prints nothing
      - diagnose(duration) - checks where it fails
      - parallel/sequential (aspects of a SUITE)
      - ignore - mark one/more test as ignored
      - success - fail all ignored tests
      - times - measure execution time of tests/suites
      - before/beforeAll + after/afterAll -passing zio that will be run before or after tests
     */
  )
}
