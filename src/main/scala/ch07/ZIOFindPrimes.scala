package ch07

import zio._
import zio.clock._
import zio.console._

import java.util.concurrent.TimeUnit
import scala.annotation.tailrec
import scala.language.postfixOps
import scala.util.Random

trait PrimeCalculator {
  def isPrime(n: Int): Boolean
}

class TextBookCalculator extends PrimeCalculator {
  def isPrime(n: Int): Boolean =
    (2 until n) forall (d => n % d != 0L)
}

/** https://dev.to/guildenstern70/a-pure-functional-primality-test-in-scala-3gif */
class Guildenstern70Calculator extends PrimeCalculator {
  def isPrime(n: Int): Boolean =
    isPrime(n, 5)

  private def isPrime(n: Int, i: Int): Boolean = {
    if (n <= 3)
      return n > 1
    if (n % 2 == 0 || n % 3 == 0)
      return false
    while (i * i <= n) {
      if (n % i == 0 || n % (i + 2) == 0)
        return false
      return isPrime(n, i + 6)
    }
    true
  }
}

class RecursiveCalculator extends PrimeCalculator {
  def isPrime(n: Int): Boolean = {
    @tailrec
    def loop(n: Int, i: Int = 5): Boolean =
      if (i * i <= 5)
        if (n % i == 0 || n % (i + 2) == 0) false
        else
          loop(n, i + 6)
      else
        true

    if (n <= 3) n > 1
    else if (n % 2 == 0 || n % 3 == 0) false
    else loop(n)
  }
}

object PrimeCalculator {
  def apply(name: String): PrimeCalculator =
    name match {
      case "textbook"       => new TextBookCalculator()
      case "guildenstern70" => new Guildenstern70Calculator()
      case "recursive"      => new RecursiveCalculator()
      case unknown          => throw new IllegalArgumentException(s"Unsupported calculator: $unknown")
    }
}

object ZIOFindPrimes extends App {

  val rand = new Random()

  def isPrimeZIO(n: Int)(isPrime: Int => Boolean): UIO[Boolean] =
    ZIO.effectTotal(isPrime(n))

  def identifyPrimes(v: Vector[Int])(isPrime: Int => Boolean): UIO[Vector[Int]] =
    ZIO.filterPar(v)(n => isPrimeZIO(n)(isPrime))

  def splitVector(v: Vector[Int], n: Int = 20): Vector[Vector[Int]] =
    v.grouped(n).toVector

  def generateNumbers(n: Int): UIO[Vector[Int]] =
    ZIO.effectTotal((1 to n).map(_ => Math.abs(rand.nextInt)).toVector)

  def findPrimes(randomNums: Vector[Int], numFibers: Int)(isPrime: Int => Boolean): UIO[(Long, Int)] =
    for {
      start  <- Clock.Service.live.currentTime(TimeUnit.MILLISECONDS)
      primes <- ZIO.collectParN(numFibers)(splitVector(randomNums, numFibers))(v => identifyPrimes(v)(isPrime))
      end    <- Clock.Service.live.currentTime(TimeUnit.MILLISECONDS)
    } yield (end - start, primes.flatten.size)

  def findOptimalNumFibers(randomNums: Vector[Int], numFibers: Int)(isPrime: Int => Boolean): UIO[Int] = {

    def isGoodEnough(curr: Double, prev: Double) =
      curr < prev && Math.abs(curr * curr - prev) / prev < 0.1

    def iter(currNumFibers: Int, prevTimeInMillis: Long): UIO[Int] = {
      findPrimes(randomNums, currNumFibers)(isPrime).flatMap {
        case (currTimeInMillis, numPrimes) =>
          println(
            f"""Using $currNumFibers%2d fiber(s) took $currTimeInMillis%6d ms
               |to find $numPrimes prime(s) out of ${randomNums.size} numbers""".stripMargin.replace('\n', ' ')
          )
          if (isGoodEnough(currTimeInMillis, prevTimeInMillis)) ZIO.succeed(currNumFibers)
          else if (currTimeInMillis >= prevTimeInMillis) ZIO.succeed(currNumFibers - 1)
          else iter(currNumFibers + 1, currTimeInMillis)
      }
    }

    iter(numFibers, Long.MaxValue)
  }

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {
    val numPrimes       = args.headOption.map(_.toInt).getOrElse(100)
    val primeCalculator = PrimeCalculator(args.lastOption.getOrElse("textbook"))

    (for {
      _             <- putStrLn(s"Running with $numPrimes primes and ${primeCalculator.getClass.getSimpleName}")
      randomNumbers <- generateNumbers(numPrimes)
      numFibers     <- findOptimalNumFibers(randomNumbers, 1)(primeCalculator.isPrime)
      _             <- putStrLn(s"Optimal number of fibers was $numFibers")
    } yield ()).exitCode
  }
}
