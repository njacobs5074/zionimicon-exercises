package ch02

import zio._
import zio.console._

import java.io.IOException

object Ex19 extends App {

  def checkText(acceptInput: String => Boolean): ZIO[Console, IOException, (Boolean, String)] =
    for {
      text <- getStrLn
      accepted <- ZIO.effectTotal(acceptInput(text))
    } yield (accepted, text)

  def readUntil(acceptInput: String => Boolean): ZIO[Console, IOException, String] =
    checkText(acceptInput).flatMap {
      case (true, text) => ZIO.succeed(text)
      case _            => readUntil(acceptInput)
    }

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
    readUntil(s => s == "Hello").exitCode
}
