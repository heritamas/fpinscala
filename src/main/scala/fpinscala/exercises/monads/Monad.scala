package fpinscala.exercises
package monads

import parsing.*
import testing.*
import parallelism.*
import state.*
import parallelism.Par.*

import scala.collection.mutable.ListBuffer

trait Functor[F[_]]:
  extension [A](fa: F[A])
    def map[B](f: A => B): F[B]

  extension [A, B](fab: F[(A, B)]) def distribute: (F[A], F[B]) =
    (fab.map(_(0)), fab.map(_(1)))

  extension [A, B](e: Either[F[A], F[B]]) def codistribute: F[Either[A, B]] =
    e match
      case Left(fa) => fa.map(Left(_))
      case Right(fb) => fb.map(Right(_))

object Functor:
  given listFunctor: Functor[List] with
    extension [A](as: List[A])
      def map[B](f: A => B): List[B] = as.map(f)

trait Monad[F[_]] extends Functor[F]:
  def unit[A](a: => A): F[A]

  extension [A](fa: F[A])
    def flatMap[B](f: A => F[B]): F[B] =
      fa.map(f).join

    def map[B](f: A => B): F[B] =
      fa.flatMap(a => unit(f(a)))

    def map2[B, C](fb: F[B])(f: (A, B) => C): F[C] =
      fa.flatMap(a => fb.map(b => f(a, b)))

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(identity)


  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List.empty[B]))( ( a, acc ) => f(a).map2(acc)(_ :: _))


  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a =>
      for
        b <- f(a)
        c <- g(b)
      yield
        c

  extension [A](fa: F[A])
    def flatMapViaCompose[B](f: A => F[B]): F[B] =
      compose(_ => fa, f)(None)

  def filterM_[A](as: List[A])(f: A => F[Boolean]): F[List[A]] =
    val tmp = traverse(as){ a => f(a).flatMap(b => if b then unit(Some(a)) else unit(None)) }
    tmp.map(la => la.collect{ case Some(a) => a })

  def filterM[A](as: List[A])(f: A => F[Boolean]): F[List[A]] =
    as.foldRight(unit(List.empty[A])){
      (a, acc) => f(a).flatMap(b => if b then unit(a).map2(acc)(_ :: _) else acc)
    }


  extension [A](ffa: F[F[A]])
    def join: F[A] =
      ffa.flatMap(identity)

  extension [A](fa: F[A])
    def flatMapViaJoinAndMap[B](f: A => F[B]): F[B] =
      fa.map(f).join

  def composeViaJoinAndMap[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => f(a).map(g).join

end Monad

object Monad:
  given genMonad: Monad[Gen] with
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    extension [A](fa: Gen[A])
      override def flatMap[B](f: A => Gen[B]): Gen[B] =
        Gen.flatMap(fa)(f)

  given parMonad: Monad[Par] with
    def unit[A](a: => A) = Par.unit(a)
    extension [A](fa: Par[A])
      override def flatMap[B](f: A => Par[B]): Par[B] =
        Par.flatMap(fa)(f)

  def parserMonad[P[+_]](p: Parsers[P]): Monad[P] = new:
    def unit[A](a: => A) = p.succeed(a)
    extension [A](fa: P[A])
      override def flatMap[B](f: A => P[B]): P[B] =
        p.flatMap(fa)(f)


  given optionMonad: Monad[Option] with
    def unit[A](a: => A) = Some(a)
    extension [A](fa: Option[A])
      override def flatMap[B](f: A => Option[B]) =
        fa.flatMap(f)

  given lazyListMonad: Monad[LazyList] with
    def unit[A](a: => A) = LazyList(a)
    extension [A](fa: LazyList[A])
      override def flatMap[B](f: A => LazyList[B]) =
        fa.flatMap(f)

  given listMonad: Monad[List] with
    def unit[A](a: => A) = List(a)
    extension [A](fa: List[A])
      override def flatMap[B](f: A => List[B]) =
        fa.flatMap(f)

end Monad

case class Id[+A](value: A):
  def map[B](f: A => B): Id[B] =
    Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] =
    f(value)

object Id:
  given idMonad: Monad[Id] with
    def unit[A](a: => A): Id[A] = Id(a)
    extension [A](fa: Id[A])
      override def flatMap[B](f: A => Id[B]): Id[B] =
        fa.flatMap(f)


opaque type State[S, +A] = S => (A, S)

object State:
  def modify[S](f: S => S): State[S, Unit] = s => ((), f(s))
  def get[S]: State[S, S] = s => (s, s)
  def set[S](s: => S): State[S, Unit] = _ => ((), s)

  /*
  extension [S, A](underlying: State[S, A])
    def map[B](f: A => B): State[S, B] =
      flatMap(a => s => (f(a), s))

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      s =>
        val (a, s1) = underlying(s)
        f(a)(s1)
  */

  given stateMonad[S]: Monad[[x] =>> State[S, x]] with
    def unit[A](a: => A): State[S, A] = s => (a, s)
    extension[A] (st: State[S, A] )
      override def flatMap[B](f: A => State[S, B]): State[S, B] =
        s =>
          val (a, s1) = st(s)
          f(a)(s1)
        //State.flatMap(st)(f)

def zipWithIndex[A](as: List[A]): List[(A, Int)] =
  import State.*
  import State.given
  val statefulList : ListBuffer[State[Int, (A, Int)]] = ListBuffer.from(as).map {
    a =>
      for
        index <- get
        _ <- set(index + 1)
      yield (a, index)
  }

  val combinedState : State[Int, List[(A, Int)]] =
    statefulList.foldLeft(stateMonad.unit(List.empty[(A, Int)])) {
      (acc, state) =>
        for
          xs <- acc
          x <- state
        yield xs.appended(x)
    }

  combinedState(0)._1

opaque type Reader[-R, +A] = R => A

object Reader:
  extension [R, A](ra: Reader[R, A])
    def run(r: R): A = ra(r)

  given readerMonad[R]: Monad[Reader[R, _]] with
    def unit[A](a: => A): Reader[R, A] = _ => a
    extension [A](fa: Reader[R, A])
      override def flatMap[B](f: A => Reader[R, B]) =
        r => f(fa(r))(r)
