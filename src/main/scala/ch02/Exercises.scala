package ch02

import zio._
import zio.console.{getStrLn, putStr}

import scala.concurrent.{ExecutionContext, Future}

/**
  * @author nick
  * @since 2021/07/23
  */
object Exercises extends App {
  // Ex 1
  def readFile(file: String): String = {
    val source = scala.io.Source.fromFile(file)
    try source.getLines.mkString("\n")
    finally source.close()
  }

  def readFileZio(file: String): UIO[String] = ZIO.effectTotal(readFile(file))

  // Ex 2
  def writeFile(file: String, text: String): Unit = {
    import java.io._
    val pw: PrintWriter = if (file == "-") {
      new PrintWriter(System.out)
    } else {
      new PrintWriter(new File(file))
    }
    try {
      pw.write(text)
      pw.flush()
    } finally if (file != "-") pw.close()
  }

  def writeFileZio(file: String, text: String): UIO[Unit] =
    ZIO.effectTotal(writeFile(file, text))

  // Ex 3
  def copyFileZio(source: String, dest: String): ZIO[Any, Nothing, Unit] =
    readFileZio(source).flatMap(s => writeFileZio(dest, s))

  /* Ex 4
   * Rewrite using for comprehension:
      def printLine(line: String) = ZIO.effect(println(line))
      val readLine = ZIO.effect(scala.io.StdIn.readLine())
      printLine("What is your name?").flatMap(_ => readLine.flatMap(name => printLine(s"Hello, ${name}!")))
   */
  def whatIsYourName(): Task[Unit] = {
    def printLine(line: String): Task[Unit] = ZIO.effect(println(line))
    val readLine: Task[String] = ZIO.effect(scala.io.StdIn.readLine())

    for {
      _ <- printLine("What is your name? ")
      name <- readLine
      _ <- printLine(s"Hello, $name")
    } yield ()

  }

  /* Ex5
   * Rewrite the following ZIO code that uses flatMap into a for comprehension.
    val random = ZIO.effect(scala.util.Random.nextInt(3) + 1)
    def printLine(line: String) = ZIO.effect(println(line)) val readLine = ZIO.effect(scala.io.StdIn.readLine())
      random.flatMap { int =>
       printLine("Guess a number from 1 to 3:").flatMap { _ =>
        readLine.flatMap { num =>
          if (num == int.toString)
            printLine("You guessed right!")
         else
           printLine(s"You guessed wrong, the number was $int!")
       }
     }
    }
   */
  def guessNumber(): Task[Unit] = {
    val random = ZIO.effect(scala.util.Random.nextInt(3) + 1)
    def printLine(line: String): Task[Unit] = ZIO.effect(println(line))
    val readLine: Task[String] = ZIO.effect(scala.io.StdIn.readLine())

    for {
      int <- random
      _ <- printLine("Guess a number from 1 to 3: ")
      num <- readLine.map(_.toInt)
      _ <-
        if (num == int) printLine("You guessed right!")
        else printLine(s"You guessed wrong: the number was $int")
    } yield ()

  }

  // Ex 11 - p58
  def eitherToZIO[E, A](either: Either[E, A]): ZIO[Any, E, A] =
    either.fold(ZIO.fail(_), ZIO.succeed(_))

  // Ex 12 - p58
  def listToZIO[A](list: List[A]): ZIO[Any, None.type, A] = ZIO.succeed(list.head)

  // Ex 13 - p58
  def currentTime(): Long = System.currentTimeMillis()
  lazy val currentTimeZIO: ZIO[Any, Nothing, Long] = ZIO.effectTotal(currentTime())

  // Ex 14 - p58
  def getCacheValue(key: String)(callback: Either[Throwable, String] => Unit): Unit = ???

  def getCacheValueZio(key: String): ZIO[Any, Throwable, String] =
    ZIO.effectAsync { callback =>
      getCacheValue(key) {
        case Right(value) => callback(ZIO.succeed(value))
        case Left(t)      => callback(ZIO.fail(t))
      }
    }

  // Ex 15 - p 58-9
  trait User

  def saveUserRecord(user: User)(cb: Either[Throwable, Unit] => Unit): Unit = ???

  def saveUserRecordZio(user: User): ZIO[Any, Throwable, Unit] =
    ZIO.effectAsync { callback =>
      saveUserRecord(user) {
        case Right(()) => callback(ZIO.succeed(()))
        case Left(t)   => callback(ZIO.fail(t))
      }
    }

  // Ex 16 - p 59
  trait Query
  trait Result

  def doQuery(query: Query)(implicit ec: ExecutionContext): Future[Result] = ???

  def doQueryZio(query: Query): ZIO[Any, Throwable, Result] =
    Task.fromFuture(implicit ec => doQuery(query))

  // Ex 20 - p 61
  def doWhile[R, E, A](body: ZIO[R, E, A])(condition: A => Boolean): ZIO[R, E, A] =
    body.filterOrElse(a => !condition(a))(_ => doWhile(body)(condition))

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
    doWhile(getStrLn)(text => text != "Hello").exitCode

}
