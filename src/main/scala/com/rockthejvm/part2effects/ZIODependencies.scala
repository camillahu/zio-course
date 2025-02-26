package com.rockthejvm.part2effects

import zio._

object ZIODependencies extends ZIOAppDefault {

  //app to subscribe users to newsletter

  case class User(name: String, email: String)

  class UserSubscription(emailService: EmailService, userDatabase: UserDatabase) {
    def subscribeUser(user: User): Task[Unit] = for {
      _ <- emailService.email(user)
      _ <- userDatabase.insert(user)
    } yield ()
  }

  object UserSubscription {
    def create(emailService: EmailService, userDatabase: UserDatabase) =
      new UserSubscription(emailService, userDatabase)
  }

  class EmailService {
    def email(user: User): Task[Unit] = {
      ZIO.succeed(println(s"you've just been subscribed. Welcome, ${user.name}!"))
    }
  }

  object EmailService {
    def create(): EmailService = new EmailService
  }

  class UserDatabase(connectionPool: ConnectionPool) {
    def insert(user: User): Task[Unit] = for {
      conn <- connectionPool.get
      _ <- conn.runQuery(s"insert into subscribers(name, email) values (${user.name}, ${user.email}")
    } yield ()
  }

  object UserDatabase {
    def create(connectionPool: ConnectionPool) =
      new UserDatabase(connectionPool)
  }

  class ConnectionPool(nConnections: Int) {
    def get: Task[Connection] = {
      ZIO.succeed(println("Aquired connection")) *> ZIO.succeed(Connection())
    }
  }

  object ConnectionPool {
    def create(nConnections: Int) =
      new ConnectionPool(nConnections)
  }

  case class Connection() {
    def runQuery(query: String): Task[Unit] = {
      ZIO.succeed(println(s"Executing query: $query"))
    }
  }

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

  val userSubscriptionLayer: ZLayer[Any, Nothing, UserSubscription] =
    subscriptionRequirementsLayer >>> userSubscriptionServiceLayer
    //mix & match




  def run = program_v2.provideLayer(userSubscriptionLayer)

}