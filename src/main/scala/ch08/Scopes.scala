package ch08

import zio._
import zio.clock._
import zio.duration._

/**
  * @author nick
  * @since 2021/08/09
  */
object Scopes extends App {

  def effect(
      scope: ZScope[Exit[Any, Any]]
  ): ZIO[Clock, Nothing, Int] = {
    for {
      _ <-
        ZIO
          .effectTotal(print("💗"))
          .delay(1.second)
          .forever
          .forkIn(scope)
      _ <- ZIO.effectTotal(println("Doing some expensive work..."))
    } yield 42
  }

  val module: ZIO[Any with Clock, Nothing, Int] =
    for {
      fiber <- ZIO.forkScopeWith(scope => effect(scope).fork)
      _ <-
        ZIO
          .effectTotal("Doing some other work")
          .delay(5.seconds)
      result <- fiber.join
    } yield result

  val program: ZIO[Any with Clock, Nothing, Unit] = for {
    fiber <- module.fork
    _ <-
      ZIO
        .effectTotal(println("Running another module entirely"))
        .delay(10.seconds)
    _ <- fiber.join
  } yield ()

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
    program.exitCode
}
