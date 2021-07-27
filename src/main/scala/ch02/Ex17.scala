package ch02

import zio._
import zio.console._

object Ex17 extends App {

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {
    (for {
      _ <- putStr("What's your name? ")
      name <- getStrLn
      _ <- putStrLn(s"Nice to meet you, $name")
    } yield ()).exitCode
  }
}
