package com.rockthejvm.part2effects

import zio.*

object ZIOApps {

  val meaningOfLife: UIO[Int] = ZIO.succeed(42)

  def main(args: Array[String]): Unit = {
    val runtime = Runtime.default
    given trace: Trace = Trace.empty
    Unsafe.unsafeCompat { unsafe =>
      given u: Unsafe = unsafe

      println(runtime.unsafe.run(meaningOfLife))
    }
  }
}

object BetterApp extends ZIOAppDefault {
  //provides runtime and trace
//  override def run = ZIOApps.meaningOfLife.flatMap(mol => ZIO.succeed(println(mol)))
  override def run = ZIOApps.meaningOfLife.debug //will print result
}

//not needed - best to use default
object ManualApp extends ZIOApp {

  override implicit def environmentTag: zio.EnvironmentTag[ManualApp.type] = ???

  override type Environment = this.type

  override def bootstrap: ZLayer[ZIOAppArgs with Scope, Any, ManualApp.type] = ???

  override def run: ZIO[ManualApp.type with ZIOAppArgs with Scope, Any, Any] = ???
}
