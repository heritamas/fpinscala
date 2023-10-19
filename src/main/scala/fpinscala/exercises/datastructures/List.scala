package fpinscala.exercises.datastructures

/** `List` data type, parameterized on a type, `A`. */
enum List[+A]:
  /** A `List` data constructor representing the empty list. */
  case Nil
  /** Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
    which may be `Nil` or another `Cons`.
   */
  case Cons(head: A, tail: List[A])

object List: // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.

  def product(doubles: List[Double]): Double = doubles match
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  @annotation.nowarn // Scala gives a hint here via a warning, so let's disable that
  val result = List(1,2,3,4,5) match
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))

  def foldRight[A,B](as: List[A], acc: B, f: (A, B) => B): B = // Utility functions
    as match
      case Nil => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def sumViaFoldRight(ns: List[Int]): Int =
    foldRight(ns, 0, (x,y) => x + y)

  def productViaFoldRight(ns: List[Double]): Double =
    foldRight(ns, 1.0, _ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = l match
    case Nil => sys.error("empty list")
    case Cons(_, t) => t

  def setHead[A](l: List[A], h: A): List[A] = l match
    case Nil => sys.error("empty list")
    case Cons(_, t) => Cons(h, t)

  def drop[A](l: List[A], n: Int): List[A] =
    if n <= 0 then l
    else l match
      case Nil => Nil
      case Cons(_, t) => drop(t, n-1)

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l

  def init[A](l: List[A]): List[A] = l match
    case Nil => sys.error("empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))

  def length[A](l: List[A]): Int =
    import scala.annotation.tailrec
    @tailrec
    def lenacc[A](l: List[A], len : Int) : Int = l match
      case Nil => len
      case Cons(h, t) => lenacc(t, len+1)

    lenacc(l, 0)


  def foldLeft[A,B](l: List[A], acc: B, f: (B, A) => B): B =
    import scala.annotation.tailrec
    @tailrec
    def foldLeftWithAcc(l: List[A], f: (B, A) => B, result: B) : B = l match
      case Nil => result
      case Cons(h, t) => foldLeftWithAcc(t, f, f(result, h))

    foldLeftWithAcc(l, f, acc)

  /*
  def foldLeft[A, B](l: List[A], acc: B, f: (B, A) => B): B = l match
    case Nil => acc
    case Cons(h, t) => foldLeft(t, f(acc, h), f)
   */

  private def flip[A, B, C](f: (A, B) => C): (B, A) => C = (b, a) => f(a, b)
  def foldRightWithLeft[A,B](as: List[A], acc: B, f: (A, B) => B): B =
    foldLeft(reverse(as), acc, flip(f))

  def foldRightViaFoldLeft_[A, B](l: List[A], acc: B, f: (A, B) => B): B =
    foldLeft(l, (b: B) => b, (g, a) => b => g(f(a, b)))(acc)

  def foldLeftViaFoldRight_[A, B](l: List[A], acc: B, f: (B, A) => B): B =
    foldRight(l, (b: B) => b, (a, g) => b => g(f(b, a)))(acc)


  def sumViaFoldLeft(ns: List[Int]): Int = foldLeft(ns, 0, _ + _)

  def productViaFoldLeft(ns: List[Double]): Double = foldLeft(ns, 1.0, _ * _)

  def lengthViaFoldLeft[A](l: List[A]): Int = foldLeft(l, 0, (acc, h) => acc + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A](), (rev, h) => Cons(h, rev))

  def appendViaFoldRight_1[A](l: List[A], r: List[A]): List[A] =
    reverse(foldRight(reverse(r), reverse(l), (e, l) => Cons(e, l)))

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r, Cons(_,_))


  def concat[A](l: List[List[A]]): List[A] =
    foldLeft(l, Nil: List[A], append)

  def incrementEach(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int], (h, l) => Cons(h+1, l))

  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String], (d, l) => Cons(d.toString, l) )

  def map[A,B](l: List[A], f: A => B): List[B] = ???

  def filter[A](as: List[A], f: A => Boolean): List[A] = ???

  def flatMap[A,B](as: List[A], f: A => List[B]): List[B] = ???

  def filterViaFlatMap[A](as: List[A], f: A => Boolean): List[A] = ???

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = ???

  // def zipWith - TODO determine signature

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = ???
