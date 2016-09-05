package fpinscala.laziness

import Stream._

import scala.collection.mutable.ListBuffer
trait Stream[+A] {

  def toList: List[A] = {
    @annotation.tailrec
    def loop(s: Stream[A], l: List[A]): List[A] = s match {
      case Cons(h, t) => loop(t(), h() :: l)
      case _ => l
    }

    loop(this, List()).reverse
  }

  def toListWithMutable: List[A] = {
    val buf = new ListBuffer[A]
    @annotation.tailrec
    def loop(s: Stream[A]): List[A] = s match {
      case Cons(h,t) => buf.append(h()); loop(t())
      case _ => buf.toList
    }
    loop(this)
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 =>
      Stream.cons(h(), t().take(n-1))
    case Cons(h,_) if n == 0 => Stream.cons(h(), Stream.empty)
    case _ => Empty
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n-1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
    case _ => Empty
  }

  def forAll(p: A => Boolean): Boolean = foldRight[Boolean](true)((v,acc) => p(v) && acc)

  def headOption: Option[A] = foldRight[Option[A]](None)((h,_) => Some(h))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = {
    foldRight[Stream[B]](Empty)((el, s) => cons(f(el),s))
  }

  def filter[B](p: A => Boolean): Stream[A] = {
    foldRight(empty[A])((el,s) => if (p(el)) cons(el, s) else s)
  }

  def append[B>:A](other: => Stream[B]): Stream[B] = {
    foldRight(other)((h,t) => cons(h,t))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(empty[B])((a,b) => f(a).append(b))
  }

  def mapUnfold[B](f: A => B): Stream[B] = {
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }
  }

  def takeUnfold(n: Int): Stream[A] = unfold((this,n)) {
    case (Cons(h, t), 1) => Some((h(), (empty,0)))
    case (Cons(h, t), m) if m > 1 => Some((h(), (t(), m-1)))
    case _ => None
  }

  def takeWhileUnfold(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if p(h()) => Some((h(), t()))
    case _ => None
  }

  def zipWithUnfold[B,C](other: Stream[B])(f: (A,B) => C): Stream[C] = unfold((this,other)) {
    case (Cons(h1,t1), Cons(h2,t2)) => Some((f(h1(), h2()), (t1(), t2())))
    case _ => None
  }

  def zipAllUnfold[B](other: Stream[B]): Stream[(Option[A],Option[B])] = unfold((this,other)) {
    case (Empty,Empty) => None
    case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(),empty))
    case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (empty, t2()))
    case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()),Some(h2())),(t1(),t2())))
  }

  def startsWith[B>:A](s: Stream[B]): Boolean = zipAllUnfold(s).foldRight(true) {
    case ((Some(t),Some(o)),b) => b && t == o
    case ((Some(t),None),b) => b
    case ((None, None),_) => true
    case _ => false
  }

  def startsWithAlt[B>:A](s: Stream[B]): Boolean = zipAllUnfold(s).takeWhile(_._2.isDefined).forAll({
    case (a,b) => a == b
  })

  def tails: Stream[Stream[A]] = unfold(this) {
    case s@Cons(_,t) => Some((s,t()))
    case Empty => None
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def constant(n: Int): Stream[Int] = {
    lazy val ns:Stream[Int] = Cons(() => n, () => ns)
    ns
  }

  def from(n: Int): Stream[Int] = {
    cons(n, from(n+1))
  }

  def fibs(): Stream[Int] = {
    def go(n2: Int, n1: Int): Stream[Int] = {
      cons(n2,go(n1,n2+n1))
    }
    go(0,1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z).map(as => cons(as._1, unfold(as._2)(f))).getOrElse(empty[A])
  }

  def fibsunfold(): Stream[Int] = {
    unfold((0,1))(s => Some((s._1, (s._2, s._1 + s._2))))
  }

  def fromunfold(n: Int): Stream[Int] = {
    unfold(n)(i => Some((i,i+1)))
  }

  def constantunfold(n: Int): Stream[Int] = {
    unfold(n)(_ => Some((n,n)))
  }

  def onesunfold(n: Int): Stream[Int] = {
    unfold(1)(_ => Some((1,1)))
  }

}