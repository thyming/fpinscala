package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) =>
        println(h)
        Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) =>
        f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Cons(_,t) => t
    case _ => Nil
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Cons(_,t) => Cons(h,t)
    case _ => Nil
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_,t) => drop(t,n-1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Cons(h,t) if f(h) => dropWhile(t,f)
      case _ => l
    }
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(h,Nil) => Nil
      case Cons(h,t) => Cons(h,init(t))
    }
  }

  def length[A](l: List[A]): Int = {
    foldRight(l,0)((_,b)=>b+1)
  }

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(h,t) => foldLeft(t, f(z,h))(f)
    }
  }

  def sumfold[A <: Int](l: List[A]): Int = {
    foldLeft(l,0)(_ + _)
  }

  def productfold(l: List[Double]): Double = foldLeft(l,1.0)(_ * _)

  def lengthfold[A](l: List[A]): Int = foldLeft(l,0)((s,_)=>s+1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l,Nil:List[A])((el,t) => Cons(t,el))

  def foldLeftInTermsOfFoldRight[A,B](l:List[A],z: B)(f: (B,A) => B): B = {
    foldRight(l, identity[B](_))((listEl,g) => accum => g(f(accum,listEl)))(z)
  }

  def foldRightInTermsOfFoldLeft[A,B](l:List[A],z:B)(f: (A,B) => B): B = {
    foldLeft(l, identity[B](_))((g,listEl) => accum => g(f(listEl,accum)))(z)
  }

  def appendfold[A](l1:List[A], l2: List[A]): List[A] = {
    foldRight(l1,l2)((a,b) => Cons(a,b))
  }

  def flattenlists[A](lol: List[List[A]]): List[A] = {
    foldRight(lol, Nil:List[A])(append)
  }

  def add1toEach(l: List[Int]): List[Int] = {
    foldRight(l,Nil:List[Int])((a,bs)=> Cons(a+1,bs))
  }

  def strigifyEachDouble(l: List[Double]): List[String] = {
    foldRight(l, Nil: List[String])((a, bs) => Cons(a.toString, bs))
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = {
    foldRight(l, Nil:List[B])((a, bs) => Cons(f(a),bs))
  }

  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    foldRight(l, Nil:List[A])((a, bs) => {
      if (f(a)) Cons(a,bs)
      else bs
    })
  }

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = {
    flattenlists(map(l)(f))
  }

  def filterFlatMap[A](l: List[A])(f: A => Boolean): List[A] = {
    flatMap(l)(a => if(f(a)) Cons(a,Nil) else Nil)
  }

  def sumElementsInLockstep(l1: List[Int], l2: List[Int]): List[Int] = {
    (l1,l2) match {
      case (Nil, _) => Nil
      case (Cons(_,_), Nil) => Nil
      case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2,sumElementsInLockstep(t1,t2))
    }
  }

  def zipWith[A,B,C](l1: List[A], l2: List[B])(f: (A,B) => C): List[C] = {
    (l1,l2) match {
      case (Nil, _) => Nil
      case (Cons(_,_), Nil) => Nil
      case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2),zipWith(t1,t2)(f))
    }
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    @annotation.tailrec
    def startsWith(sup: List[A], sub: List[A]): Boolean = {
      (sup, sub) match {
        case (_, Nil) => true
        case (Cons(h1,t1),Cons(h2,t2)) => h1 == h2 && startsWith(t1,t2)
        case _ => false
      }
    }

    sup match {
      case Nil => sub == Nil
      case _ if startsWith(sup,sub) => true
      case Cons(h,t) => hasSubsequence(t,sub)
    }
  }

}
