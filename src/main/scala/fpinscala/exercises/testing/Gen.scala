package fpinscala.exercises.testing

import fpinscala.exercises.state.*
import fpinscala.exercises.parallelism.*
import fpinscala.exercises.parallelism.Par.Par
import Gen.*
import Prop.*

import java.util.concurrent.{ExecutorService, Executors}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/


type Gen[+A] = State[RNG, A]

trait Prop


object Prop:
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???


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


/*

trait Gen[A]:
  def map[B](f: A => B): Gen[B] = ???
  def flatMap[B](f: A => Gen[B]): Gen[B] = ???
*/
