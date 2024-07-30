package fpinscala.exercises.monoids

import fpinscala.exercises.parallelism.Nonblocking.*

trait Monoid[A]:
  def combine(a1: A, a2: A): A
  def empty: A

object Monoid:

  val stringMonoid: Monoid[String] = new:
    def combine(a1: String, a2: String) = a1 + a2
    val empty = ""

  def listMonoid[A]: Monoid[List[A]] = new:
    def combine(a1: List[A], a2: List[A]) = a1 ++ a2
    val empty = Nil

  lazy val intAddition: Monoid[Int] = new:
    def combine(a1: Int, a2: Int) = a1+a2
    val empty = 0

  lazy val intMultiplication: Monoid[Int] = new:
    def combine(a1: Int, a2: Int) = a1*a2
    val empty = 1

  lazy val booleanOr: Monoid[Boolean] = new:
    def combine(a1:Boolean, a2:Boolean) = a1 || a2
    val empty = false

  lazy val booleanAnd: Monoid[Boolean] = new:
    def combine(a1:Boolean, a2:Boolean) = a1 && a2
    val empty = true

  def optionMonoid[A]: Monoid[Option[A]] = new:
    def combine(a1:Option[A], a2:Option[A]) = a1.orElse(a2)
    val empty = Option.empty[A]

  def dual[A](m: Monoid[A]): Monoid[A] = new:
    def combine(x: A, y: A): A = m.combine(y, x)
    val empty = m.empty

  def endoMonoid[A]: Monoid[A => A] = new:
    def combine(a1:A=>A, a2:A=>A) = a1.andThen(a2)
    val empty = identity[A]

  import fpinscala.answers.testing.{Prop, Gen}
  import Gen.`**`

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    val associativity = Prop
      .forAll(gen ** gen ** gen):
        case a ** b ** c =>
          m.combine(a, m.combine(b, c)) == m.combine(m.combine(a, b), c)
      .tag("associativity")
    val identity = Prop
      .forAll(gen): a =>
        m.combine(a, m.empty) == a && m.combine(m.empty, a) == a
      .tag("identity")
    associativity && identity

  def combineAll[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.empty)(m.combine)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.map(f).foldLeft(m.empty)(m.combine)

  def foldRight[A, B](as: List[A])(acc: B)(f: (A, B) => B): B =
    foldMap(as, dual(endoMonoid))(f.curried)(acc)

  def foldLeft[A, B](as: List[A])(acc: B)(f: (B, A) => B): B =
    foldMap(as, endoMonoid)(a => b => f(b, a))(acc)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    as.length match {
      case 0 => m.empty
      case 1 => f(as(0))
      case _ =>
        val (l, r) = as.splitAt(as.length/2)
        m.combine(foldMapV(l,m)(f), foldMapV(r,m)(f))
    }

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new:
    def combine(a1: Par[A], a2: Par[A]): Par[A] = a1.map2(a2)(m.combine)
    val empty = Par.unit(m.empty)

  // simpler
  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = 
    Par.parMap(v)(f).map{ bs => bs.foldLeft(m.empty)(m.combine) }


  def parFoldMap_[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = 
    Par.parMap(v)(f).flatMap: bs =>
        foldMapV(bs, par(m))(b => Par.lazyUnit(b))

  case class Interval(ordered: Boolean, min: Int, max: Int)
  val orderedMonoid: Monoid[Option[Interval]] = new:
    def combine(oa1: Option[Interval], oa2: Option[Interval]) =
      (oa1, oa2) match
        case (Some(a1), Some(a2)) =>
          Some(Interval(a1.ordered && a2.ordered && a1.max <= a2.min,
            a1.min, a2.max))
        case (x, None) => x
        case (None, x) => x
    val empty = None

  def ordered(ints: IndexedSeq[Int]): Boolean =
    foldMapV(ints, orderedMonoid)(i => Some(Interval(true, i, i)))
      .map(_.ordered).getOrElse(true)

  enum WC:
    case Stub(chars: String)
    case Part(lStub: String, words: Int, rStub: String)

  val wcMonoid: Monoid[WC] = new Monoid[WC]:
    val empty = WC.Stub("")

    def combine(wc1: WC, wc2: WC) = (wc1, wc2) match
      case (WC.Stub(a), WC.Stub(b)) => WC.Stub(a + b)
      case (WC.Stub(a), WC.Part(l, w, r)) => WC.Part(a + l, w, r)
      case (WC.Part(l, w, r), WC.Stub(a)) => WC.Part(l, w, r + a)
      case (WC.Part(l, w, r), WC.Part(l2, w2, r2)) =>
        WC.Part(l, w + (if (r + l2).isEmpty then 0 else 1) + w2, r2)

  def count(s: String): Int =
    // A single character's count. Whitespace does not count,
    // and non-whitespace starts a new Stub.
    def wc(c: Char): WC =
      if c.isWhitespace then
        WC.Part("", 0, "")
      else
        WC.Stub(c.toString)
    def unstub(s: String) = if s.isEmpty then 0 else 1

    foldMapV(s.toIndexedSeq, wcMonoid)(wc) match
      case WC.Stub(s) => unstub(s)
      case WC.Part(l, w, r) => unstub(l) + w + unstub(r)


  given productMonoid[A, B](using ma: Monoid[A], mb: Monoid[B]): Monoid[(A, B)] with
    def combine(x: (A, B), y: (A, B)) = (ma.combine(x._1, y._1), mb.combine(x._2, y._2))
    val empty = (ma.empty, mb.empty)

  given functionMonoid[A, B](using mb: Monoid[B]): Monoid[A => B] with
    def combine(f: A => B, g: A => B) = a => mb.combine(f(a), g(a))
    val empty: A => B = a => mb.empty

  given mapMergeMonoid[K, V](using mv: Monoid[V]): Monoid[Map[K, V]] with
    def combine(a: Map[K, V], b: Map[K, V]) =
      (a.keySet ++ b.keySet).foldLeft(empty): (acc,k) =>
        acc.updated(k, mv.combine(a.getOrElse(k, mv.empty),
                                  b.getOrElse(k, mv.empty)))
    val empty = Map()

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    val bagMonoid = mapMergeMonoid[A, Int](using intAddition)
    foldMapV(as, bagMonoid)(a => Map(a -> 1))


end Monoid
