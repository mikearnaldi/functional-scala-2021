enum Effect[-R, +E, +A]:
  def map[B](f: A => B): Effect[R, E, B] = this.flatMap(a => Effect.succeed(f(a)))

  def flatMap[R2, E2, B](f: A => Effect[R2, E2, B]): Effect[R2 & R, E2 | E, B] = Effect.FlatMap(this, f)

  def foldM[R2, E2, A2, R3, E3, A3](f: A => Effect[R2, E2, A2], g: E => Effect[R3, E3, A3]): Effect[R3 & R2 & R, E3 | E2, A3 | A2] = Effect.Fold(this, f, g)

  def inject[R2, R3](r: R2)(implicit tag: Tag[R2], ev: R2 & R3 => R): Effect[R3, E, A] = Effect.Provide(this, r, tag, ev)

  def catchAll[R2, E2, A2](f: E => Effect[R2, E2, A2]) = this.foldM(Effect.succeed, f)
  
  def catchSome[R2, E2, A2, A3 >: A | A2, E3, E4](f: PartialFunction[E, (E3, Effect[R2, E2, A2])])(implicit ev: E => E3 | E4): Effect[R & R2, E2 | E4, A3] = this.foldM(Effect.succeed, e => {
    f.lift(e) match {
      case Some((e3, io)) if e3 == e => io
      case _ => Effect.fail(ev(e).asInstanceOf[E4])
    }
  })

  case Succeed[A](
    val a: () => A
  ) extends Effect[Any, Nothing, A]
  case Fail[E](
    val e: () => E
  ) extends Effect[Any, E, Nothing]
  case Use[R, R2, E2, A](
    val f: R => Effect[R2, E2, A], 
    val tag: Tag[R]
  ) extends Effect[R, Nothing, A]
  case Provide[R, E, A, R2, R3](
    val zio: Effect[R, E, A], 
    val r: R2, 
    val tag: Tag[R2], 
    val ev: R2 & R3 => R
  ) extends Effect[R3, E, A]
  case FlatMap[R, E, A, R2, E2, A2](
    val zio: Effect[R, E, A],
    val f: A => Effect[R2, E2, A2]
  ) extends Effect[R & R2, E | E2, A2]
  case Fold[R, E, A, R2, E2, A2, R3, E3, A3](
    val zio: Effect[R, E, A],
    val f: A => Effect[R2, E2, A2],
    val g: E => Effect[R3, E3, A3]
  ) extends Effect[R & R2 & R3, E2 | E3, A2 | A3]

class Tag[A]()

enum Exit[+E, +A]:
  case Fail(e: E)
  case Succeed(a: A)

enum ContiuationFrame:
  case FlatMap(val f: Any => Effect[Any, Any, Any])
  case Fold(val f: Any => Effect[Any, Any, Any], val g: Any => Effect[Any, Any, Any])

extension [E, A](zio: Effect[Any, E, A])
  def unsafeRun: Exit[E, A] = {
    var current = zio.asInstanceOf[Effect[Any, Any, Any]]
    var result = null.asInstanceOf[Any]
    var errored = false
    var cont = List[ContiuationFrame]()
    var services = Map[Tag[Any], Any]()

    var recursing = true

    while (recursing) {
      while (current != null) {
        current match {
          case Effect.Succeed(a) => {
            result = a()
            current = null
          }
          case Effect.Fail(e) => {
            errored = true
            result = e()
            current = null
          }
          case Effect.Use(f, tag) => {
            current = f(services.getOrElse(tag, null).asInstanceOf[Any]).asInstanceOf[Effect[Any, Any, Any]]
          }
          case Effect.FlatMap(io, f) => {
            cont = ContiuationFrame.FlatMap(f.asInstanceOf[Any => Effect[Any, Any, Any]]) :: cont
            current = io
          }
          case Effect.Fold(io, f, g) => {
            cont = ContiuationFrame.Fold(f.asInstanceOf[Any => Effect[Any, Any, Any]], g.asInstanceOf[Any => Effect[Any, Any, Any]]) :: cont
            current = io
          }
          case Effect.Provide(io, r, tag, ev) => {
            val prev = services
            services = services + (tag.asInstanceOf[Tag[Any]] -> r.asInstanceOf[Any])
            current = Effect.Fold(
              io.asInstanceOf[Effect[Any, Any, Any]],
              a => {
                services = prev
                Effect.Succeed(() => a)
              },
              e => {
                services = prev
                Effect.Fail(() => e)
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

object Effect {
  def succeed[A](a: => A): Effect[Any, Nothing, A] = Succeed(() => a)

  def fail[E](e: => E): Effect[Any, E, Nothing] = Fail(() => e)

  def use[R] = new PartiallyAppliedUse[R]

  final class PartiallyAppliedUse[R](private val dummy: Boolean = true) extends AnyVal {
    def apply[R2, E2, A](f: R => Effect[R2, E2, A])(implicit tag: Tag[R]): Effect[R & R2, E2, A] = Use(f, tag)
  }
}