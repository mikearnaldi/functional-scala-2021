enum IO[-R, +E, +A]:
  def map[B](f: A => B): IO[R, E, B] = this.flatMap(a => IO.succeed(f(a)))

  def flatMap[R2, E2, B](f: A => IO[R2, E2, B]): IO[R2 & R, E2 | E, B] = IO.FlatMap(this, f)

  def foldM[R2, E2, A2, R3, E3, A3](f: A => IO[R2, E2, A2], g: E => IO[R3, E3, A3]): IO[R3 & R2 & R, E3 | E2, A3 | A2] = IO.Fold(this, f, g)

  def inject[R2, R3](r: R2)(implicit tag: Tag[R2], ev: R2 & R3 => R): IO[R3, E, A] = IO.Provide(this, r, tag, ev)

  def catchAll[R2, E2, A2](f: E => IO[R2, E2, A2]) = this.foldM(IO.succeed, f)
  
  def catchSome[R2, E2, A2 >: A, E3, E4](f: PartialFunction[E, (E3, IO[R2, E2, A2])])(implicit ev: E => E3 | E4): IO[R & R2, E2 | E4, A2] = this.foldM(IO.succeed, e => {
    f.lift(e) match {
      case Some((e3, io)) if e3 == e => io
      case _ => IO.fail(ev(e).asInstanceOf[E4])
    }
  })

  case Succeed[A](
    val a: () => A
  ) extends IO[Any, Nothing, A]
  case Fail[E](
    val e: () => E
  ) extends IO[Any, E, Nothing]
  case Use[R, R2, E2, A](
    val f: R => IO[R2, E2, A], 
    val tag: Tag[R]
  ) extends IO[R, Nothing, A]
  case Provide[R, E, A, R2, R3](
    val zio: IO[R, E, A], 
    val r: R2, 
    val tag: Tag[R2], 
    val ev: R2 & R3 => R
  ) extends IO[R3, E, A]
  case FlatMap[R, E, A, R2, E2, A2](
    val zio: IO[R, E, A],
    val f: A => IO[R2, E2, A2]
  ) extends IO[R & R2, E | E2, A2]
  case Fold[R, E, A, R2, E2, A2, R3, E3, A3](
    val zio: IO[R, E, A],
    val f: A => IO[R2, E2, A2],
    val g: E => IO[R3, E3, A3]
  ) extends IO[R & R2 & R3, E2 | E3, A2 | A3]

class Tag[A]()

enum Exit[+E, +A]:
  case Fail(e: E)
  case Succeed(a: A)

enum ContiuationFrame:
  case FlatMap(val f: Any => IO[Any, Any, Any])
  case Fold(val f: Any => IO[Any, Any, Any], val g: Any => IO[Any, Any, Any])

extension [E, A](zio: IO[Any, E, A])
  def unsafeRun: Exit[E, A] = {
    var current = zio.asInstanceOf[IO[Any, Any, Any]]
    var result = null.asInstanceOf[Any]
    var errored = false
    var cont = List[ContiuationFrame]()
    var services = Map[Tag[Any], Any]()

    var recursing = true

    while (recursing) {
      while (current != null) {
        current match {
          case IO.Succeed(a) => {
            result = a()
            current = null
          }
          case IO.Fail(e) => {
            errored = true
            result = e()
            current = null
          }
          case IO.Use(f, tag) => {
            current = f(services.getOrElse(tag, null).asInstanceOf[Any]).asInstanceOf[IO[Any, Any, Any]]
          }
          case IO.FlatMap(io, f) => {
            cont = ContiuationFrame.FlatMap(f.asInstanceOf[Any => IO[Any, Any, Any]]) :: cont
            current = io
          }
          case IO.Fold(io, f, g) => {
            cont = ContiuationFrame.Fold(f.asInstanceOf[Any => IO[Any, Any, Any]], g.asInstanceOf[Any => IO[Any, Any, Any]]) :: cont
            current = io
          }
          case IO.Provide(io, r, tag, ev) => {
            val prev = services
            services = services + (tag.asInstanceOf[Tag[Any]] -> r.asInstanceOf[Any])
            current = IO.Fold(
              io.asInstanceOf[IO[Any, Any, Any]],
              a => {
                services = prev
                IO.Succeed(() => a)
              },
              e => {
                services = prev
                IO.Fail(() => e)
              }
            )
          }
        }
      }

      var popping = true

      while (popping) {
        cont match {
          case head :: tail => {
            cont = tail
            
            head match {
              case ContiuationFrame.FlatMap(f) => {
                if (!errored) {
                  current = f(result)
                  popping = false
                }
              }
              case ContiuationFrame.Fold(f, g) => {
                if (errored) {
                  errored = false
                  current = g(result)
                } else {
                  current = f(result)
                }
                popping = false
              }
            }
          }
          case _ => {
            popping = false
          }
        }
      }

      recursing = current != null
    }

    if (errored) {
      Exit.Fail(result.asInstanceOf[E])
    } else {
      Exit.Succeed(result.asInstanceOf[A])
    }
  }

object IO {
  def succeed[A](a: => A): IO[Any, Nothing, A] = Succeed(() => a)

  def fail[E](e: => E): IO[Any, E, Nothing] = Fail(() => e)

  def use[R, R2, E2, A](f: R => IO[R2, E2, A])(implicit tag: Tag[R]): IO[R & R2, E2, A] = Use(f, tag)
}