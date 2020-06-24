trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

case class Pure[S[_], A](a: A) extends Free[S, A]
case class Suspend[S[_], A](a: S[A]) extends Free[S, A]
case class FlatMapped[S[_], B, C](c: Free[S, C], f: C => Free[S, B]) extends Free[S, B]

sealed abstract class Free[S[_], A] extends Product with Serializable {

  final def map[B](f: A => B): Free[S, B] = flatMap(a => Pure(f(a)))
  final def flatMap[B](f: A => Free[S, B]): Free[S, B] = FlatMapped(this, f)

  @annotation.tailrec
  final def resume(implicit S: Functor[S]): Either[S[Free[S, A]], A] =
    this match {
      case Pure(a)    => Right(a)
      case Suspend(t) => Left(S.map(t)(Pure(_)))
      case FlatMapped(c, f) =>
        c match {
          case Pure(a)          => f(a).resume
          case Suspend(t)       => Left(S.map(t)(f))
          case FlatMapped(d, g) => d.flatMap(dd => g(dd).flatMap(f)).resume
        }
    }
}
