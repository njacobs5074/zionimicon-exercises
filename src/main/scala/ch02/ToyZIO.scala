package ch02

/**
  * Ex 6
  * Implement the zipWith function in terms of the toy model of a ZIO effect.
  *
  * The function should return an effect that sequentially composes the specified effects,
  * merging their results with the specified user-defined function.
  */
object ToyZIO extends App {

  final case class ZIO[-R, +E, +A](run: R => Either[E, A]) {

    def map[B](f: A => B): ZIO[R, E, B] =
      ZIO(r => run(r).map(f))

    def flatMap[R1 <: R, E1 >: E, B](f: A => ZIO[R1, E1, B]): ZIO[R1, E1, B] =
      ZIO(r => run(r).fold(ZIO.fail(_), f).run(r))

  }

  object ZIO {
    def effect[A](a: => A): ZIO[Any, Throwable, A] =
      ZIO(_ =>
        try Right(a)
        catch {
          case t: Throwable => Left(t)
        }
      )

    def fail[E](e: => E): ZIO[Any, E, Nothing] =
      ZIO(_ => Left(e))

    // Ex 6 - p57
    def zipWith[R, E, A, B, C](self: ZIO[R, E, A], that: ZIO[R, E, B])(
        f: (A, B) => C
    ): ZIO[R, E, C] =
      self.flatMap(a => that.map(b => f(a, b)))

    // Ex 7 - p57
    def collectAll[R, E, A](in: Iterable[ZIO[R, E, A]]): ZIO[R, E, List[A]] =
      ZIO(r => Right(in.map(_.run(r)).map { case Right(a) => a }.toList))

    // Ex 8 - p57
    def foreach[R, E, A, B](in: Iterable[A])(f: A => ZIO[R, E, B]): ZIO[R, E, List[B]] =
      collectAll(in.map(f).toList)

    // Ex 9 - p57-8
    def orElse[R, E1, E2, A](self: ZIO[R, E1, A], that: ZIO[R, E2, A]): ZIO[R, E2, A] =
      ZIO(r => self.run(r).fold(_ => that.run(r), selfResult => Right(selfResult)))
  }

  val zio1 = ZIO[Any, Nothing, String](run = _ => Right("Hello"))
  val zio2 = ZIO[Any, Nothing, String](run = _ => Right("World"))
  val zio3 = ZIO[Any, Throwable, String](run = _ => Left(throw new Error("Boom!")))

  //ZIO.zipWith(zio1, zio2) { case (a, b) => println(s"$a $b") }.run()
  //ZIO.zipWith(zio2, zio3) { case (a, b) => println(s"$a $b") }.run()

  //ZIO.collectAll(List(zio1, zio2, zio3)).run().foreach(l => println(l.mkString(" ")))
  //ZIO.foreach(List("Hello", "ZIO", throw new Error("Boom!")))(s => ZIO.effect(println(s))).run()

  ZIO.orElse(ZIO.effect(println(42 / 0)), ZIO.effect(println("No worries, all is well"))).run()
  ZIO
    .orElse(
      ZIO.effect(println("All good here, too")),
      ZIO.effect(throw new Error("Never even got here"))
    )
    .run()
}
