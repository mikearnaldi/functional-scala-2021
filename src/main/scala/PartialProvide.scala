import zio.*

object Experimental {
  extension [R, E, A](self: ZIO[R, E, A])
    def providePartial[R1, E1 >: E, R2, R3 >: R](layer: ZLayer[R1, E1, R2])(implicit ev: R2 & R3 => R): ZIO[R3 & R1, E1, A] = ???

  val clockLayer: ZLayer[Any, Nothing, Clock]    = Clock.live
  val zio: ZIO[Clock with Random, Nothing, Unit] = ZIO.unit
  val zio2                                       = zio.providePartial(clockLayer)
  val zio3: URIO[Random, Unit]                   = zio2
}