//
// USAGE
//

trait Console {
  def putStrLn(msg: => String): Effect[Any, Nothing, Unit]
}

object Console {
  implicit val tag: Tag[Console] = Tag()

  def putStrLn(msg: => String) = Effect.use[Console](_.putStrLn(msg))
}

trait Math {
  def add(x: Int, y: Int): Effect[Any, Nothing, Int]
}

object Math {
  implicit val tag: Tag[Math] = Tag()

  def add(x: Int, y: Int) = Effect.use[Math](_.add(x, y))
}

case class ErrorA(a: String)
case class ErrorB(b: String)

val program = for {
  _ <- Console putStrLn "hello"
  _ <- Console putStrLn "world"
  y <- Math add(2, 3)
  _ <- Console putStrLn s"result: $y"
  _ <- Effect fail ErrorA("a")
  _ <- Effect fail ErrorB("b")
} yield ()

val afterHandlingErrorA = program catchSome {
  case x: ErrorA => (x, Console putStrLn "recovered from A")
}

val afterInjectingConsole = afterHandlingErrorA inject new Console {
  def putStrLn(msg: => String) = Effect succeed println(msg)
}

val main = afterInjectingConsole inject new Math {
  def add(x: Int, y: Int) = Effect succeed x + y
}

@main def root() = {
  main.unsafeRun match {
    case Exit.Fail(e) => {
      println("Error:")
      println(e)
      ()
    }
    case Exit.Succeed(a) => ()
  }
}