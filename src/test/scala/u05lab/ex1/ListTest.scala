package u05lab.ex1


import org.junit.Assert.{assertEquals, assertThrows}
import org.junit.Test

class ListTest {

  import u05lab.ex1.List._

  val list: List[Int] = List(1, 2, 3, 4)

  @Test
  def testZipRight(): Unit =
    assertEquals(List((1, 0), (2, 1), (3, 2), (4, 3)), list.zipRight)

  @Test
  def testPartition(): Unit =
    assertEquals((List(1, 3), List(2, 4)), list.partition(_ % 2 == 1))

  @Test
  def testPartition2(): Unit =
    assertEquals((List(1, 3), List(2, 4)), list.partition2(_ % 2 == 1))

  @Test
  def testSpan(): Unit =
    assertEquals((List(1), List(2, 3, 4)), list.span(_ % 2 != 0))

  @Test
  def testSpan2(): Unit =
    assertEquals((List(1, 2), List(3, 4)), list.span(_ < 3))

  @Test
  def testReduce(): Unit =
    assertEquals(10, list.reduce(_ + _))

  @Test
  def testReduceError(): Unit =
    assertThrows(classOf[UnsupportedOperationException], () => Nil[Int]().reduce(_ + _))

  @Test
  def testTake(): Unit =
    assertEquals(List(2, 3, 4), list.takeRight(3))

}
