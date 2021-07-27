package ch02

import zio.{App => ZIOApp, _}
import zio.console._
import Exercises.{readFileZio, writeFileZio}

object Cat extends ZIOApp {

  def printFiles(fileNames: List[String]): UIO[Unit] = {
    def printFile(fileName: String): UIO[Unit] =
      readFileZio(fileName).flatMap(content => writeFileZio("-", s"$content\n"))

    fileNames match {
      case Nil => ZIO.succeed(())
      case head :: tail =>
        printFile(head).zipRight(printFiles(tail))
    }
  }

  def run(commandLineArguments: List[String]): URIO[zio.ZEnv, ExitCode] =
    (if (commandLineArguments.isEmpty) {
       putStrLn("usage: Cats file1 {...fileN}")
     } else {
       printFiles(commandLineArguments)
     }).exitCode
}
