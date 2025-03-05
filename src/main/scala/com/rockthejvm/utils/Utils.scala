package com.rockthejvm.utils
import zio.ZIO

extension [R,E,A](zio: ZIO[R,E,A])
  def debugThread: ZIO[R,E,A] =
    zio
      .tap(a => ZIO.succeed(println(s"[${Thread.currentThread().getName}] $a")))
      .tapErrorCause(cause => ZIO.succeed(println(s"[${Thread.currentThread().getName}] [FAIL] $cause")))
  // Using ZIO.tap we can peek into a success value and perform any effectful operation, without changing the returning value of the original effect