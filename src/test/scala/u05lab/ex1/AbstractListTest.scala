package u05lab.ex1

import org.junit.Assert.{assertEquals, assertThrows}
import org.junit.Test
import org.junit.function.ThrowingRunnable

abstract class AbstractListTest:

  import u05lab.ex1.List._

  protected val list: List[Int] = List(1, 2, 3, 4)

  protected def testZipRight(test: List[(Int, Int)]): Unit =
    assertEquals(List((1, 0), (2, 1), (3, 2), (4, 3)), test)

  protected def testPartition(test: (Int => Boolean) => (List[Int], List[Int])): Unit =
    assertEquals((List(1, 3), List(2, 4)), test(_ % 2 == 1))

  protected def testSpan(test: (Int => Boolean) => (List[Int], List[Int])): Unit =
    assertEquals((List(1), List(2, 3, 4)), test(_ % 2 != 0))

  protected def testSpan2(test: (Int => Boolean) => (List[Int], List[Int])): Unit =
    assertEquals((List(1, 2), List(3, 4)), test(_ < 3))

  protected def testReduce(test: ((Int, Int) => Int) => Int): Unit =
    assertEquals(10, test(_ + _))

  protected def testReduceError(statement: ((Int, Int) => Int) => Unit): Unit =
    assertThrows(classOf[UnsupportedOperationException], () => statement(_ + _))

  protected def testTake(test: Int => u05lab.ex1.List[Int]): Unit =
    assertEquals(List(2, 3, 4), test(3))

  protected def testCollect(test: PartialFunction[Int, Int] => u05lab.ex1.List[Int]): Unit =
    val fun: PartialFunction[Int, Int] = {
      case x if (x % 2) == 0 => x * 2
    }
    assertEquals(List(4, 8), test(fun))

