package u05lab.ex1

import u05lab.ex1.List

// Ex 1. implement the missing methods both with recursion or with using fold, map, flatMap, and filters
// List as a pure interface
enum List[A]:
  case ::(h: A, t: List[A])
  case Nil()

  def ::(h: A): List[A] = List.::(h, this)

  def head: Option[A] = this match
    case h :: t => Some(h)
    case _ => None

  def tail: Option[List[A]] = this match
    case h :: t => Some(t)
    case _ => None

  def append(list: List[A]): List[A] = this match
    case h :: t => h :: t.append(list)
    case _ => list

  def foreach(consumer: A => Unit): Unit = this match
    case h :: t => consumer(h); t.foreach(consumer)
    case _ =>

  def get(pos: Int): Option[A] = this match
    case h :: t if pos == 0 => Some(h)
    case h :: t if pos > 0 => t.get(pos - 1)
    case _ => None

  def filter(predicate: A => Boolean): List[A] = this match
    case h :: t if predicate(h) => h :: t.filter(predicate)
    case _ :: t => t.filter(predicate)
    case _ => Nil()

  def map[B](fun: A => B): List[B] = this match
    case h :: t => fun(h) :: t.map(fun)
    case _ => Nil()

  def flatMap[B](f: A => List[B]): List[B] =
    foldRight[List[B]](Nil())(f(_) append _)

  def foldLeft[B](z: B)(op: (B, A) => B): B = this match
    case h :: t => t.foldLeft(op(z, h))(op)
    case Nil() => z

  def foldRight[B](z: B)(f: (A, B) => B): B = this match
    case h :: t => f(h, t.foldRight(z)(f))
    case _ => z

  def length: Int = foldLeft(0)((l, _) => l + 1)

  def isEmpty: Boolean = this match
    case Nil() => true
    case _ => false

  def reverse(): List[A] = foldLeft[List[A]](Nil())((l, e) => e :: l)

  /** EXERCISES */

  def foldLR[B, C, D](lz: C, lf: C => C)(z: B, f: (A, C, B) => B): B = this match
    case h :: t => f(h, lz, t.foldLR(lf(lz), lf)(z, f))
    case _ => z

  def zipRight2: List[(A, Int)] =
    foldLR(0, _ + 1)(Nil(), (a, step, b) => (a, step) :: b)

  def zipRight: List[(A, Int)] =
    val size = length - 1
    foldRight[List[(A, Int)]](Nil())((a, b) => (a, size - b.length) :: b)

  def zipRightRec: List[(A, Int)] =
    def _zipRightRec(list: List[A])(pos: Int): List[(A, Int)] = list match
      case h :: t => (h, pos) :: _zipRightRec(t)(pos + 1)
      case _ => Nil()

    _zipRightRec(this)(0)

  def partition(pred: A => Boolean): (List[A], List[A]) = (this.filter(pred), this.filter(!pred(_)))

  def partition2(pred: A => Boolean): (List[A], List[A]) =
    foldRight((Nil[A](), Nil()))((a, b) => pred(a) match
      case true => (a :: b._1, b._2)
      case false => (b._1, a :: b._2)
    )

  def partitionRec(pred: A => Boolean): (List[A], List[A]) = this match
    case h :: t =>
      val rec = t.partitionRec(pred)
      pred(h) match
        case true => (h :: rec._1, rec._2)
        case false => (rec._1, h :: rec._2)
    case _ => (Nil(), Nil())

  def span(pred: A => Boolean): (List[A], List[A]) =
    foldRight((Nil[A](), Nil[A]()))((a, b) => pred(a) match
      case true => (a :: b._1, b._2)
      case false => (Nil(), a :: b._1.append(b._2))
    )

  def spanRec(pred: A => Boolean): (List[A], List[A]) = this match
    case h :: t if pred(h) => val rec = t.spanRec(pred); (h :: rec._1, rec._2)
    case _ => (Nil(), this)


  /** @throws UnsupportedOperationException if the list is empty */
  def reduce(op: (A, A) => A): A = this match
    case h :: t => t.foldRight(h)(op)
    case _ => throw UnsupportedOperationException()

  def reduceRec(op: (A, A) => A): A = this match
    case h :: Nil() => h
    case h :: t => op(h, t.reduceRec(op))
    case _ => throw UnsupportedOperationException()

  def takeRight(n: Int): List[A] =
    foldRight[List[A]](Nil())((a, b) => if b.length == n then b else a :: b)

  def takeRightRec(n: Int): List[A] = this match
    case h :: t if length == n => h :: t.takeRightRec(n - 1)
    case h :: t => t.takeRightRec(n)
    case _ => Nil()

  def collect[B](fun: PartialFunction[A, B]): List[B] = filter(fun.isDefinedAt).map(fun)

  def collectRec[B](fun: PartialFunction[A, B]): List[B] = this match
    case h :: t => fun.isDefinedAt(h) match
      case true => fun(h) :: t.collectRec(fun)
      case false => t.collectRec(fun)
    case _ => Nil()

// Factories
object List:

  def apply[A](elems: A*): List[A] =
    var list: List[A] = Nil()
    for e <- elems.reverse do list = e :: list
    list

  def of[A](elem: A, n: Int): List[A] =
    if n == 0 then Nil() else elem :: of(elem, n - 1)

@main def checkBehaviour(): Unit =
  val reference = List(1, 2, 3, 4)
  println(reference.zipRight) // List((1, 0), (2, 1), (3, 2), (4, 3))
  println(reference.partition(_ % 2 == 0)) // (List(2, 4), List(1, 3))
  println(reference.span(_ % 2 != 0)) // (List(1), List(2, 3, 4))
  println(reference.span(_ < 3)) // (List(1, 2), List(3, 4))
  println(reference.reduce(_ + _)) // 10
  try Nil.reduce[Int](_ + _)
  catch case ex: Exception => println(ex) // prints exception
  println(List(10).reduce(_ + _)) // 10
  println(reference.takeRight(3)) // List(2, 3, 4)
