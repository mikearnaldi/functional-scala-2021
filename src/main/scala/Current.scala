import zio.*

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

  case class ErrorA(a: String)
  case class ErrorB(b: String)

  val programWithBadInference = for {
    _ <- Console putStrLn "hello"
    _ <- Console putStrLn "world"
    y <- Math add(2, 3)
    _ <- Console putStrLn s"result: $y"
    _ <- ZIO fail ErrorA("a")
    _ <- ZIO fail ErrorB("b")
  } yield ()


  sealed trait AppErrors
  case class AppErrorA(a: String) extends AppErrors
  case class AppErrorB(b: String) extends AppErrors

  val programWithOkInference = for {
    _ <- Console putStrLn "hello"
    _ <- Console putStrLn "world"
    y <- Math add(2, 3)
    _ <- Console putStrLn s"result: $y"
    _ <- ZIO fail AppErrorA("a")
    _ <- ZIO fail AppErrorB("b")
  } yield ()
}

object RealLifeAppErrors {
  sealed trait AppError
  
  sealed trait DatabaseError extends AppError
  case class QueryFailure(msg: String) extends DatabaseError
  case class ConnectionFailure(msg: String) extends DatabaseError

  sealed trait DomainError extends AppError
  case class InvalidUserId(msg: String) extends DomainError
  case class InvalidEmail(msg: String) extends DomainError

  def validateUserId(user: User): ZIO[Any, InvalidUserId, Unit] = ???
  def validateEmail(user: User): ZIO[Any, InvalidEmail, Unit] = ???

  def validateUser(user: User) = for {
    _ <- validateUserId(user)
    _ <- validateEmail(user)
  } yield ()

  case class User(id: String, email: String)

  def saveUser: User => ZIO[Any, QueryFailure, Unit] = ???

  def createUser(id: String, email: String) = for {
    user <- ZIO.succeed(User(id, email))
    _ <- validateUser(user)
    _ <- saveUser(user)
  } yield (user)
}