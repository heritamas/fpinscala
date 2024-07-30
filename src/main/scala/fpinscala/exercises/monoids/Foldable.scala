package fpinscala.exercises.monoids

trait Foldable[F[_]]:
  import Monoid.{endoMonoid, dual}

  extension [A](as: F[A])
    def foldRight[B](acc: B)(f: (A, B) => B): B =
      ???

    def foldLeft[B](acc: B)(f: (B, A) => B): B =
      ???

    def foldMap[B](f: A => B)(using mb: Monoid[B]): B =
      ???

    def combineAll(using ma: Monoid[A]): A =
      ???

    def toList: List[A] =
      ???

object Foldable:

  given Foldable[List] with
    extension [A](as: List[A])
      override def foldRight[B](acc: B)(f: (A, B) => B) =
        as.foldRight(acc)(f)
      override def foldLeft[B](acc: B)(f: (B, A) => B) =
        as.foldLeft(acc)(f)
      override def toList: List[A] = as

  given Foldable[IndexedSeq] with
    extension [A](as: IndexedSeq[A])
      override def foldRight[B](acc: B)(f: (A, B) => B) =
        as.foldRight(acc)(f)
      override def foldLeft[B](acc: B)(f: (B, A) => B) =
        as.foldLeft(acc)(f)
      override def foldMap[B](f: A => B)(using mb: Monoid[B]): B =
        Monoid.foldMapV(as, mb)(f)

  given Foldable[LazyList] with
    extension [A](as: LazyList[A])
      override def foldRight[B](acc: B)(f: (A, B) => B) =
        as.foldRight(acc)(f)
      override def foldLeft[B](acc: B)(f: (B, A) => B) =
        as.foldLeft(acc)(f)

  import fpinscala.exercises.datastructures.Tree

  given Foldable[Tree] with
    import Tree.{Leaf, Branch}
    extension [A](as: Tree[A])
      override def foldRight[B](acc: B)(f: (A, B) => B) =
        as match {
          case Leaf(a) => f(a, acc)
          case Branch(left, right) => left.foldRight(right.foldRight(acc)(f))(f)
        }
      override def foldLeft[B](acc: B)(f: (B, A) => B) =
        as match {
          case Leaf(a) => f(acc, a)
          case Branch(left, right) => right.foldLeft(left.foldLeft(acc)(f))(f)
        }
      override def foldMap[B](f: A => B)(using mb: Monoid[B]): B =
        as match {
          case Leaf(a) => f(a)
          case Branch(left, right) => mb.combine(left.foldMap(f), right.foldMap(f))
        }

  given Foldable[Option] with
    extension [A](as: Option[A])
      override def foldRight[B](acc: B)(f: (A, B) => B) =
        as match {
          case None => acc
          case Some(value) => f(value, acc)
        }
      override def foldLeft[B](acc: B)(f: (B, A) => B) =
        as match {
          case None => acc
          case Some(value) => f(acc, value)
        }
      override def foldMap[B](f: A => B)(using mb: Monoid[B]): B =
        as match {
          case None => mb.empty
          case Some(value) => f(value)
        }
