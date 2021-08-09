package ch07

import zio._
import zio.clock._
import zio.console._

import java.util.concurrent.TimeUnit
import scala.language.postfixOps
import scala.util.Random

/**
  * @author nick
  * @since 2021/08/07
  */
object ZIOFindPrimes extends App {

  val rand = new Random()

  def isPrime(n: Int): Boolean =
    (2 until n) forall (d => n % d != 0L)

  def identifyPrimes(v: Vector[Int]): UIO[Vector[Int]] =
    ZIO.effectTotal(v.filter(isPrime))

  def splitVector(v: Vector[Int], n: Int = 20): Vector[Vector[Int]] =
    v.grouped(n).toVector

  def generateNumbers(n: Int): UIO[Vector[Int]] =
    ZIO.effectTotal((1 to n).map(_ => Math.abs(rand.nextInt)).toVector)

  def findPrimes(randomNums: Vector[Int], numFibers: Int): UIO[(Long, Int)] =
    for {
      start <- Clock.Service.live.currentTime(TimeUnit.MILLISECONDS)
      primes <- ZIO.collectParN(numFibers)(splitVector(randomNums, numFibers))(identifyPrimes)
      end <- Clock.Service.live.currentTime(TimeUnit.MILLISECONDS)
    } yield (end - start, primes.flatten.size)

  def findOptimalNumFibers(randomNums: Vector[Int], numFibers: Int): UIO[Int] = {

    def isGoodEnough(curr: Double, prev: Double) =
      curr < prev && Math.abs(curr * curr - prev) / prev < 0.1

    def iter(currNumFibers: Int, prevTimeInMillis: Long): UIO[Int] = {
      findPrimes(randomNums, currNumFibers).flatMap {
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

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
    generateNumbers(500).flatMap { randomNumbers =>
      findOptimalNumFibers(randomNumbers, 1)
        .flatMap(numFibers => putStrLn(s"Optimal number of fibers was $numFibers"))
    }.exitCode
}
