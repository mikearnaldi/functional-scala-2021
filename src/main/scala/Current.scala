import zio.*

extension [R, E, A](zio: ZIO[R, E, A])
  def provideServiceZIO[R1, E1, A1, Remainder](service: ZIO[R1, E1, A1])(
      implicit
      ev: A1 & Remainder => R,
      ni: IsNotIntersection[A1],
      tag: Tag[A1]
  ): ZIO[R1 & Remainder, E | E1, A] = ZIO
    .environmentWithZIO[R & R1](r =>
      service.flatMap(s => zio.provideEnvironment(r.add(s)))
    )
    .asInstanceOf[ZIO[R1 & Remainder, E | E1, A]]

object WithZIO {
  trait Console {
    def putStrLn(msg: => String): ZIO[Any, Nothing, Unit]
  }

  object Console {
    def putStrLn(msg: => String) = ZIO.serviceWithZIO[Console](_.putStrLn(msg))
  }

  trait Math {
    def add(x: Int, y: Int): ZIO[Any, Nothing, Int]
  }

  object Math {
    def add(x: Int, y: Int) = ZIO.serviceWithZIO[Math](_.add(x, y))
  }

  val program = for {
    _ <- Console putStrLn "hello"
    _ <- Console putStrLn "world"
    y <- Math add (2, 3)
    _ <- Console putStrLn s"result: $y"
  } yield ("ok")

  val makeMath = ZIO succeed new Math {
    def add(x: Int, y: Int): ZIO[Any, Nothing, Int] = ZIO.succeed(x + y)
  }

  val ok = program provideServiceZIO makeMath
}
