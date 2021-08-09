package ch04

import zio._
import zio.console._

object Exercises extends App {

  // Ex 1 - Fix method so that it no longer fails with defects when executed
  def failWithMessage(s: String): UIO[Nothing] =
    ZIO.succeed(throw new Error(s))

  // Ex 2
  // Using the ZIO#foldCauseM operator and the Cause#defects method,
  // implement the following function. This function should take the effect, inspect
  // defects, and if a suitable defect is found, it should recover from the error
  // with the help of the specified function, which generates a new success value
  // for such a defect.
  def recoverFromSomeDefects[R, E, A](zio: ZIO[R, E, A])(
      f: Throwable => Option[A]
  ): ZIO[R, E, A] =
    zio.foldCauseM(
      (failure: Cause[E]) => ???,
      (success: A) => ???
    )

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
    failWithMessage("Hello?").catchSomeDefect {
      case t: Throwable => putStrLn(s"Ignoring $t").as()
    }.exitCode
}
