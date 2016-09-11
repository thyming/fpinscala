package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, nrng) = rng.nextInt
    val i1 = if (i == Int.MinValue) 0 else i
    (math.abs(i), nrng)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i,rngs) = nonNegativeInt(rng)
    (i.toDouble/(Int.MaxValue.toDouble + 1),rngs)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i,r) = rng.nextInt
    val (d, r1) = double(r)
    ((i,d),r1)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i,d),r) = intDouble(rng)
    ((d,i),r)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1,d2,d3),r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count <= 0) {
      (List(), rng)
    } else {
      val (i, r1) = rng.nextInt
      val (is, r2) = ints(count - 1)(r1)
      (i :: is, r2)
    }
  }

  def betterDouble(rng: RNG): Rand[Double] = {
    map(nonNegativeInt)(i => i.toDouble/(Int.MaxValue.toDouble + 1))
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (av, ar) = ra(rng)
      val (bv, br) = rb(ar)
      (f(av,bv), br)
    }
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs match {
    case h :: t => rng => {
      val (a, r) = h(rng)
      val (as, rs) = sequence(t)(r)
      (a +: as, rs)
    }
    case Nil => r => (Nil, r)
  }

  def sequenceAlt[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.reverse.foldLeft(unit(List[A]()))((a,b) => map2(b,a)(_ :: _))
  }

  def intsWithSequence(count: Int): Rand[List[Int]] = {
    sequence(List.fill(count)(int))
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a,r) = f(rng)
    g(a)(r)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod)
      else nonNegativeLessThan(n)
    }
  }

  def mapWithFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s)(a => unit(f(a)))
  }

  def map2WithFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(a => flatMap(rb)(b => unit(f(a,b))))
  }
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = State(s => {
    val (a, st) = run(s)
    (f(a), st)
  })
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = State(s => {
    val (a, st1) = run(s)
    val (b, st2) = sb.run(st1)
    (f(a,b), st2)
  })
  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, st) = run(s)
    f(a).run(st)
  })

}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
    fs.reverse.foldLeft(unit[S, List[A]](List[A]()))((b: State[S, List[A]], s) => s.map2(b)(_ :: _))
  }

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}
object CandyMachine {
  import State._

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs.map(modify[Machine] _ compose receiveInput))
    s <- get
  } yield (s.coins, s.candies)

  def simulateMachineWithoutForComprehension(inputs: List[Input]): State[Machine, (Int, Int)] = {
    sequence(inputs.map(receiveInput).map(modify))
      .flatMap(_ => get)
      .map(m => (m.coins, m.candies))
  }

  def receiveInput = (input: Input) => (machine: Machine) =>
    (input, machine) match {
      case (Coin, m@Machine(true,_,coins)) => m.copy(locked = false, coins = coins + 1)
      case (Coin, m@Machine(false,_,_)) => m
      case (Turn, m@Machine(false,candies,_)) if candies > 0 => m.copy(candies = m.candies - 1)
  }
}
