package ch02

import zio._
import zio.console._
import zio.random._

object Ex18 extends App {

  def checkGuess(n: Int): RIO[Console with Random, Boolean] =
    for {
      _ <- putStr("Guess a number between 1 and 10: ")
      line <- getStrLn
      int <- ZIO.effect(line.toInt)
      guess <- ZIO.effect(int == n)
    } yield guess

  def readGuessOrRetry(n: Int): ZIO[Console with Random, Throwable, Boolean] =
    checkGuess(n).filterOrElse(_ == true) { _ =>
      putStrLn("Guess again: ").zipRight(readGuessOrRetry(n))
    }

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
    (for {
      n <- nextIntBounded(10)
      _ <- readGuessOrRetry(n)
      _ <- putStrLn(s"You guessed it! $n")
    } yield ()).exitCode
}
