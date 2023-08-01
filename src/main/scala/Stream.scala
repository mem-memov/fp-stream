import scala.collection.immutable.{AbstractSeq, LinearSeq}

sealed trait Stream[+A]:

  def headOption: Option[A] =

    this match
      case empty: Empty =>
        None
      case Cons(head, _) =>
        Some(head())


  // TODO: not stack-safe
  def map[B](f: A => B): Stream[B] =

    this match
      case empty: Empty =>
        empty
      case Cons(head, tail) =>
        Stream.cons(f(head()), tail().map(f))

  // TODO: not stack-safe
  def take(n: Int): Stream[A] =

    if n <= 0 then
      Stream.empty
    else
      this match
        case empty: Empty =>
          empty
        case Cons(head, tail) =>
          Stream.cons(head(), tail().take(n - 1))

  // TODO: not stack-safe
  def toList: List[A] =

    this match {
      case empty: Empty =>
        Nil
      case Cons(head, tail) =>
        head() :: tail().toList
    }


sealed class Empty extends Stream[Nothing]
sealed case class Cons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A]

object Stream:

  def empty[A]: Stream[A] =

    new Empty

  def cons[A](h: => A, t: => Stream[A]): Stream[A] =

    lazy val lazyHead = h
    lazy val lazyTail = t

    Cons[A](() => lazyHead, () => lazyTail)

  // TODO: not stack-safe
  def apply[A](as: A*): Stream[A] =

    if as.isEmpty then
      empty[A]
    else
      cons(as.head, apply(as.tail: _*))