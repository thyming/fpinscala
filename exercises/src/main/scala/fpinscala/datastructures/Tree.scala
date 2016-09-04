package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size(t: Tree[_]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }
  }

  def max(t: Tree[Int]): Int = t match {
    case Leaf(i) => i
    case Branch(l,r) => max(l) max max(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l,r) => Branch(map(l)(f),map(r)(f))
  }

  def fold[A,B](t: Tree[A])(zf: A => B)(f: (B,B) => B): B = t match {
    case Leaf(a) => zf(a)
    case Branch(l,r) => f(fold(l)(zf)(f), fold(r)(zf)(f))
  }

  def foldsize[A](t: Tree[A]): Int = {
    fold(t)(_=>1)((l,r)=>1+l+r)
  }

  def foldmax(t: Tree[Int]): Int = {
    fold(t)(i => i)((l,r) => l max r)
  }

  def folddepth[A](t: Tree[A]): Int = {
    fold(t)(_ => 1)((l,r) => 1 + l + r)
  }

  def foldmap[A,B](t: Tree[A])(f: A => B): Tree[B] = {
    fold(t)(e => Leaf(f(e)):Tree[B])(Branch(_,_))
  }

}