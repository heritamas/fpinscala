package fpinscala.exercises.testing

import fpinscala.exercises.state.*
import fpinscala.exercises.parallelism.*
import fpinscala.exercises.parallelism.Par.Par
import Gen.*
import Prop.*
import Prop.Result.{Passed, Falsified, Proved}


import java.util.concurrent.{ExecutorService, Executors}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/


type Gen[+A] = State[RNG, A]

opaque type Prop = (TestCases, RNG) => Result


object Prop:

  opaque type SuccessCount = Int
  object SuccessCount:
    extension (x: SuccessCount) def toInt: Int = x
    def fromInt(x: Int): SuccessCount = x

  opaque type TestCases = Int
  object TestCases:
    extension (x: TestCases) def toInt: Int = x
    def fromInt(x: Int): TestCases = x

  opaque type MaxSize = Int
  object MaxSize:
    extension (x: MaxSize) def toInt: Int = x
    def fromInt(x: Int): MaxSize = x

  opaque type FailedCase = String
  object FailedCase:
    extension (f: FailedCase) def string: String = f
    def fromString(s: String): FailedCase = s

  enum Result:
    case Passed
    case Falsified(failure: FailedCase, successes: SuccessCount)

    def isFalsified: Boolean = this match
      case Passed => false
      case Falsified(_, _) => true

  /* Produce an infinite random lazy list from a `Gen` and a starting `RNG`. */
  def randomLazyList[A](g: Gen[A])(rng: RNG): LazyList[A] =
    LazyList.unfold(rng)(rng => Some(g.run(rng)))


  def forAll[A](as: Gen[A])(f: A => Boolean): Prop =
    (n, rng) => randomLazyList(as)(rng).zip(LazyList.from(0)).take(n).map:
      case (a, i) =>
        try
          if f(a) then Passed else Falsified(a.toString, i)
        catch
          case e: Exception => Falsified(buildMsg(a, e), i)
    .find(_.isFalsified).getOrElse(Passed)

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  extension (self: Prop) def &&(that: Prop) : Prop =
    (n, rng) =>
      self(n, rng) match
        case Passed => that(n, rng)
        case r @ _ => r

  extension (self: Prop) def ||(that: Prop) : Prop =
    (n, rng) =>
      self(n, rng) match
        case Falsified(f, s) => that(n, rng)
        case r @ _ => r



object Gen:
  def unit[A](a: => A): Gen[A] = State.unit(a)
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start))

  def boolean: Gen[Boolean] = State(RNG.nonNegativeInt).map(_ % 2 == 0)

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    State.sequence(List.fill(n)(g))

  extension [A](self: Gen[A])
    def flatMap[B](f: A => Gen[B]): Gen[B] = State.flatMap(self)(f)

    def listOfN(size: Gen[Int]): Gen[List[A]] =
      size.flatMap(n => Gen.listOfN(n, self))

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)) : Gen[A] =
    val marker = g1._2 / (g1._2 + g2._2)
    State(RNG.double).flatMap(d => if d < marker then g1._1 else g2._1)

