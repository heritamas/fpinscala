package fpinscala.exercises.state


trait RNG:
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.

object RNG:
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng =>
      val (a, rng2) = s(rng)
      (f(a), rng2)

  def listOfInts(rng: RNG) : LazyList[Int] =
    LazyList.unfold(rng):
      case rng => Some(rng.nextInt)

  def listOfIntsAndRng(rng: RNG) : LazyList[(Int, RNG)] =
    LazyList.unfold(rng):
      case rng =>
        val (nexti, nextrng) = rng.nextInt
        Some(((nexti, nextrng), nextrng))

  def nonNegativeInt_(rng: RNG): (Int, RNG) =
    rng.nextInt match
      case (n, r) if n < 0 || n == Int.MinValue => nonNegativeInt(r)
      case (n, r) => (n, r)

  def nonNegativeInt : Rand[Int] =
    def toNonNeg(n : Int) : Int = if n < 0 then -(n+1) else n
    map[Int, Int](int)(toNonNeg)

  def double_(rng: RNG): (Double, RNG) =
    val (n, r) = nonNegativeInt(rng)
    (n.toDouble/Int.MaxValue, r)

  def double: Rand[Double] =
    def toDouble01(n: Int) : Double = n.toDouble/Int.MaxValue
    map[Int,Double](nonNegativeInt)(toDouble01)

  def map2[A, B, C](a : Rand[A], b: Rand[B])(f: (A, B) => C) : Rand[C] =
    rng0 =>
      val (ra, rng1) = a(rng0)
      val (rb, rng2) = b(rng1)
      (f(ra, rb), rng2)

  def intDouble(rng: RNG): ((Int,Double), RNG) =
    map2(int, double)((_, _))(rng)

  def doubleInt(rng: RNG): ((Double,Int), RNG) =
    map2(double, int)((_, _))(rng)

  def double3(rng: RNG): ((Double,Double,Double), RNG) =
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)

  def ints_(count: Int)(rng: RNG): (List[Int], RNG) =
    val res0 = listOfIntsAndRng(rng).take(count).toList
    res0.foldLeft((List.empty[Int], rng))( (res, irpair) => (irpair._1 :: res._1, irpair._2))

  //def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = ???

  def sequence_[A](rs: List[Rand[A]]): Rand[List[A]] =
    rng =>
      rs.foldLeft((List.empty[A], rng)):
        case ((lst, lstrng), rand) =>
          val (rndval, nextrng) = rand(lstrng)
          ((rndval :: lst), nextrng)

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
    rs.foldRight(unit(List.empty[A]))( (r, acc) => map2(r, acc)(_ :: _))

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    sequence(List.fill(count)(int))(rng)

  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] =
    rng =>
      val (a, rnga) = r(rng)
      f(a)(rnga)

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt):
        i =>
          val mod = i % n
          if i + (n - 1) - mod >= 0 then unit(mod)
          else nonNegativeLessThan(n)

  def mapViaFlatMap[A, B](r: Rand[A])(f: A => B): Rand[B] =
    flatMap(r)( a => unit(f(a)) )

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb) ( b => f(a, b)) )


opaque type State[S, +A] = S => (A, S)

object State:
  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

    def map[B](f: A => B): State[S, B] =
      s =>
        val (a, sn) = run(s)
        (f(a), sn)

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      s =>
        val (a, sn) = underlying(s)
        val (b, snn) = sb(sn)
        (f(a, b), snn)

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      s =>
        val (a, sn) = underlying(s)
        f(a)(sn)

  def apply[S, A](f: S => (A, S)): State[S, A] = f

  def unit[S, A](a: => A): State[S, A] =
    s => (a, s)

  def derived[S, A](f: S => A): State[S, A] =
    s => (f(s), s)

  def sequence[S, A](rs: List[State[S, A]]): State[S, List[A]] =
    rs.foldRight(unit(List.empty[A]))((r, acc) => r.map2(acc)(_ :: _))

  def sequence[S, A](rs: List[State[S, A]], f: S => List[A]): State[S, List[A]] =
    rs.foldRight(derived(f))((r, acc) => r.map2(acc)(_ :: _))

enum Input:
  case Coin, Turn

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy:

  def resultFromMachine(m: Machine) = ((m.coins, m.candies), m)

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    State.sequence(
      inputs.map:
        case Input.Coin => insertCoin
        case Input.Turn => turnKnob
      ,
      m => List(resultFromMachine(m)._1)
    ).map(_.last)


  /*
      S: Machine
      A: (candies, coins)
   */
  def insertCoin : State[Machine, (Int, Int)] = State {
      case m @ Machine(_, 0, _) => resultFromMachine(m)
      case m @ Machine(false, _, _) => resultFromMachine(m)
      case m @ Machine(true, _, _) => resultFromMachine(m.copy(locked = false, coins = m.coins + 1))
    }

  def turnKnob : State[Machine, (Int, Int)] = State {
      case m @ Machine(_, 0, _) => resultFromMachine(m)
      case m @ Machine(true, _, _) => resultFromMachine(m)
      case m @ Machine(false, candies, _) => resultFromMachine(m.copy(locked = true, candies = m.candies - 1))
    }


