package com.rockthejvm.part2effects

import com.rockthejvm.part2effects.ZIODependencies.User
import zio._

object ZIODependenciesServiceLayer {
  class UserSubscription(emailService: EmailService, userDatabase: UserDatabase) {
    def subscribeUser(user: User): Task[Unit] = for {
      _ <- emailService.email(user)
      _ <- userDatabase.insert(user)
    } yield ()
  }

  object UserSubscription {
    def create(emailService: EmailService, userDatabase: UserDatabase) = {
      new UserSubscription(emailService, userDatabase)
    }

    val live: ZLayer[EmailService with UserDatabase, Nothing, UserSubscription] =
      ZLayer.fromFunction(create _)
  }

  class EmailService {
    def email(user: User): Task[Unit] = {
      ZIO.succeed(println(s"you've just been subscribed. Welcome, ${user.name}!"))
    }
  }

  object EmailService {
    def create(): EmailService = new EmailService

    val live: ZLayer[Any, Nothing, EmailService] =
      ZLayer.fromFunction(create _)
  }

  class UserDatabase(connectionPool: ConnectionPool) {
    def insert(user: User): Task[Unit] = for {
      conn <- connectionPool.get
      _ <- conn.runQuery(s"insert into subscribers(name, email) values (${user.name}, ${user.email}")
    } yield ()
  }

  object UserDatabase {
    def create(connectionPool: ConnectionPool) = new UserDatabase(connectionPool)

    val live: ZLayer[ConnectionPool, Nothing, UserDatabase] =
      ZLayer.fromFunction(create _)
  }

  class ConnectionPool(nConnections: Int) {
    def get: Task[Connection] = {
      ZIO.succeed(println("Aquired connection")) *> ZIO.succeed(Connection())
    }
  }

  object ConnectionPool {
    def create(nConnections: Int) = new ConnectionPool(nConnections)

    def live(nConnections: Int): ZLayer[Any, Nothing, ConnectionPool] =
      ZLayer.succeed(create(nConnections))
    //different from the rest because requiring int as a dependency doesn't make sense in this case
  }


  case class Connection() {
    def runQuery(query: String): Task[Unit] = {
      ZIO.succeed(println(s"Executing query: $query"))
    }
  }
}
