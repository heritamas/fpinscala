package fpinscala.exercises.datastructures

import fpinscala.exercises.datastructures.Tree.Branch

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int = this match
    case Leaf(_) => 1
    case Branch(l, r) => 1 + l.size + r.size

  def depth: Int = this match
    case Leaf(_) => 0
    case Branch(left, right) => 1 + ( left.depth max right.depth )

  def map[B](f: A => B): Tree[B] = this match
    case Leaf(v) => Leaf(f(v))
    case Branch(left, right) => Branch(left.map(f), right.map(f))

  def fold[B](f: A => B, g: (B,B) => B): B = this match
    case Leaf(v) => f(v)
    case Branch(left, right) => g(left.fold(f, g), right.fold(f, g))
  
  def sizeViaFold: Int = fold(Function.const(1), 1 + _ + _)
  
  def depthViaFold: Int = fold(Function.const(0), (l, r) => 1 + l.max(r))
  
  def mapViaFold[B](f: A => B): Tree[B] = fold(f andThen Leaf.apply, Branch.apply)

object Tree:

  def size[A](t: Tree[A]): Int = t match
    case Leaf(_) => 1
    case Branch(l,r) => 1 + size(l) + size(r)

  extension (t: Tree[Int]) def firstPositive: Int = t match
    case Leaf(i) => i
    case Branch(left, right) =>
      val l = left.firstPositive
      if l > 0 then l else right.firstPositive

  extension (t: Tree[Int]) def maximum: Int = t match
    case Leaf(v) => v
    case Branch(left, right) => left.maximum max right.maximum

  extension (t: Tree[Int]) def maximumViaFold: Int = t.fold(identity, _ max _)
