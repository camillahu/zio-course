package com.rockthejvm.part2effects

import com.rockthejvm.part2effects.ZIODependenciesServiceLayer.*
import zio.*

import java.util.concurrent.TimeUnit

object ZIODependencies extends ZIOAppDefault {

  //app to subscribe users to newsletter

  case class User(name: String, email: String)


  val subscriptionService = ZIO.succeed( // Dependency injection
    UserSubscription.create(
      EmailService.create(),
      UserDatabase.create(
        ConnectionPool.create(10)
      )
    )
  )

  /*
  above is a "clean" Dependency injection, but has a bunch of drawbacks.
  - does not scale for many services.
  - DI can be much worse
    - pass dependencies partially
    - not having all dependencies in the same place.
    - passing dependencies multiple times
  */

  //example problem for passing dependencies like this:
  def subscribe(user: User): ZIO[Any, Throwable, Unit] = for {
    sub <- subscriptionService //service is instantiated at the point of call(will be called many times if many users)
    _ <- sub.subscribeUser(user)
  } yield ()

  //risk leaking resources if you subscribe multiple users in the same program.
  val program = for {
    _ <- subscribe(User("Daniel", "daniel@rtjvm.com"))
    _ <- subscribe(User("Bon Jovi", "jon@rtjvm.com"))
  } yield ()

  //alternative - different type signature and use of the built-in ZIO.service
  //will only be able to be called if somebody pushes a UserSubscription to it.
  def subscribe_v2(user: User): ZIO[UserSubscription, Throwable, Unit] = for {
    sub <- ZIO.service[UserSubscription] // ZIO[UserSubscription, Nothing, UserSubscription]
    _ <- sub.subscribeUser(user)
  } yield ()

  val program_v2 = for {
    _ <- subscribe_v2(User("Daniel", "daniel@rtjvm.com"))
    _ <- subscribe_v2(User("Bon Jovi", "jon@rtjvm.com"))
  } yield ()

  //  def run = program_v2.provideLayer { //"provide" for scala 2
  //    ZLayer.succeed(
  //      UserSubscription.create(
  //        EmailService.create(),
  //        UserDatabase.create(
  //          ConnectionPool.create(10)
  //        )
  //      )
  //    )
  //  }

  /*
    - we don't need to care about dependencies until the end of the world
    - all ZIOs requiring this dependency will use the same instance
    - can use different instances of the same type for different needs (e.g. testing)
    - layers can be created and composed much like regular ZIOs + rich API
   */

  // ------------------------------------------------------
  // ZLayers
  // ------------------------------------------------------

  val connectionPoolLayer: ZLayer[Any, Nothing, ConnectionPool] =
    ZLayer.succeed(ConnectionPool.create(10))
    //very similar syntax to regular zio. Provides connection pool when invoked.

  val databaseLayer: ZLayer[ConnectionPool, Nothing, UserDatabase] =
    ZLayer.fromFunction(UserDatabase.create _)
    //fetches args from UserDatabase.create function(ConnectionPool) and puts those args as dependencies in the ZLayer.
  val emailServiceLayer: ZLayer[Any, Nothing, EmailService] =
    ZLayer.succeed(EmailService.create())
  val userSubscriptionServiceLayer: ZLayer[EmailService with UserDatabase, Nothing, UserSubscription] =
    ZLayer.fromFunction(UserSubscription.create _)

    //composing layers -- provide a service with all the layers set
  val databaseLayerFull: ZLayer[Any, Nothing, UserDatabase] =
    connectionPoolLayer >>> databaseLayer
    //this is called vertical composition
    //combining db and conn layers -- "feeding" the conn layer to db layer with >>>

  val subscriptionRequirementsLayer: ZLayer[Any, Nothing, UserDatabase with EmailService] =
    databaseLayerFull ++ emailServiceLayer
    //horizontal composition -- both dependencies and values will be combined.
    //also combines the error channel to the lowest common ancestor of both.
    //combining layers so that return type is both UserDatabase and EmailService.
    //can use "with" keyword in both, or "&" in Scala 3.

  val userSubscriptionLayer: ZLayer[UserDatabase with EmailService, Nothing, UserSubscription] =
    subscriptionRequirementsLayer >>> userSubscriptionServiceLayer
    //mix & match

  // best practice: write "factory" methods exposing layers in the companion objects of the services
  val runnableProgram = program_v2.provideLayer(userSubscriptionLayer)

  // -----------------------------------------------------------------------
  //TIPS AND TRICKS
  // -----------------------------------------------------------------------

  // magic -> if you miss a dependency, ZIO will tell you which one(s) with this approach:
  val runnableProgram_v2 = program_v2.provide(
    UserSubscription.live,
    EmailService.live,
    UserDatabase.live,
    ConnectionPool.live(10),
  ) //will also tell you if you've added multiple layers of the same type
  // ZLayer.Debug.tree can give you insight on the structure of dependencies.
  // ZLayer.Debug.mermaid can give you insight on the structure of dependencies by giving a link to open in browser.


  //magic v2 ->
  val userSubscriptionLayer_v2:ZLayer[Any, Nothing, UserSubscription] = ZLayer.make[UserSubscription] (
    UserSubscription.live,
    EmailService.live,
    UserDatabase.live,
    ConnectionPool.live(10),
  ) // ZIO will figure out which layer fits in where.

  // passthrough -- take a dependency and also expose it in the value channel.
  val dbWithPoolLayer: ZLayer[ConnectionPool, Nothing, ConnectionPool with UserDatabase] = {
    UserDatabase.live.passthrough
  }

  //service -- take a dependency and expose it as a value to further layers
  val dbService = ZLayer.service[UserDatabase]

  //launch -- "start" a service, creates a ZIO that uses the services and never finishes
  val subscriptionLaunch: ZIO[EmailService with UserDatabase, Nothing, Nothing] = UserSubscription.live.launch

  //memoization --hidden feature of ZLayers: when an instance is instantiated, the same instance will be used further.
  //don't need to create a new instance every time it is used. Active by default, unless you use the modifier .fresh


  // Already provided services with ZIOAppDefault: Clock, Random, System, Console
    val getTime = Clock.currentTime(TimeUnit.SECONDS)
    val randomValue = Random.nextInt
    val sysVariable = System.env("HADOOP_HOME")
    val printlnEffect = Console.printLine("This is ZIO print line")



  def run = runnableProgram_v2

}
