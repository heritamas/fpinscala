package fpinscala.exercises.laziness

import fpinscala.exercises.laziness.LazyList.{empty, unfold}


enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  import fpinscala.exercises.laziness.LazyList.cons
  def toList: List[A] = this match
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)

  def take(n: Int): LazyList[A] = this match
    case Empty => Empty
    case _ if n == 0 => empty
    case Cons(h, t) => cons(h(), t().take(n-1))

  def drop(n: Int): LazyList[A] = this match
    case Cons(h, t) if n > 0 => t().drop(n-1)
    case _ => this

  def takeWhile(p: A => Boolean): LazyList[A] = this match
    case Cons(h, t) if p(h()) => cons(h() , t().takeWhile(p))
    case _ => empty

  def forAll(p: A => Boolean): Boolean = this match
    case Empty => true
    case Cons(h, t) => p(h()) && t().forAll(p)

  def forAll_1(p: A => Boolean): Boolean = foldRight(true)((hv, tv) => p(hv) && tv)

  def takeWhile_1(p: A => Boolean): LazyList[A] =
    foldRight(empty)( (h, l) => if p(h) then cons(h, l) else empty)

  def headOption: Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f : A => B) : LazyList[B] =
    foldRight(empty[B])( (h, l) => cons(f(h), l))

  def filter(f: A => Boolean): LazyList[A] =
    foldRight(empty[A])( (h, l) => if f(h) then cons(h, l) else l)

  def append[A2>:A](that: => LazyList[A2]): LazyList[A2] =
    foldRight(that)( (h, l) => cons(h, l))

  def flatMap[B](f: A => LazyList[B]): LazyList[B] =
    foldRight(empty[B])( (a, acc) => f(a).append(acc))

  def mapViaUnfold[B](f : A => B) : LazyList[B] =
    unfold(this):
      case Empty => None
      case Cons(h, t) => Some((f(h()), t()))

  def takeViaUnfold(n: Int): LazyList[A] =
    unfold((this, n)):
      case (Cons(h, t), n) if n > 0 => Some(h(), (t(), n-1))
      case _ => None

  def takeWhileViaUnfold(p: A => Boolean): LazyList[A] =
    unfold(this):
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None

  def zipAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] =
    unfold((this, that)):
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some((Some(h()), None), (t(), Empty))
      case (Empty, Cons(h, t)) => Some((None, Some(h())), (Empty, t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))

  def zipWith[B, C](that: LazyList[B])(f: (A, B) => C): LazyList[C] =
    unfold((this, that)):
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
      case _ => None

  def startsWith[B](s: LazyList[B]): Boolean =
    zipAll(s).takeWhile(_(1).isDefined).forAll((a1, a2) => a1 == a2)

  def tails : LazyList[LazyList[A]] = this match
    case Empty => LazyList(empty)
    case Cons(h, t) => cons(this, t().tails)

  def hasSubsequence[B](l: LazyList[B]): Boolean =
    tails.exists(_.startsWith(l))

  def scanRight[B](init: B)(f: (A, => B) => B): LazyList[B] =
    foldRight((init, LazyList(init))):
      case (last, (lastbutone, acc)) =>
        val newlast = f(last, lastbutone)
        (newlast, cons(newlast, acc))
    ._2

object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] = 
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty 
    else cons(as.head, apply(as.tail*))

  val ones: LazyList[Int] = LazyList.cons(1, ones)

  def continually[A](a: A): LazyList[A] = cons(a, continually(a))

  def from(n: Int): LazyList[Int] = cons(n, from(n+1))

  lazy val fibs: LazyList[Int] =
    def iter(curr: Int, next: Int) : LazyList[Int] =
      cons(curr, iter(next, curr + next))

    iter(0, 1)


  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] = f(state) match
    case None => LazyList.empty[A]
    case Some((a, st)) => cons(a, unfold(st)(f))


  lazy val fibsViaUnfold: LazyList[Int] =
    unfold((0, 1))( lasttwo => { val res = lasttwo._1+lasttwo._2; Some((lasttwo._1, (lasttwo._2, res))) } )

  def fromViaUnfold(n: Int): LazyList[Int] =
    unfold(n)(n => Some((n, n+1)))

  def continuallyViaUnfold[A](a: A): LazyList[A] =
    unfold(a)( a => Some((a,a)))

  lazy val onesViaUnfold: LazyList[Int] =
    continuallyViaUnfold(1)

