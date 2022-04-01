package u05lab.ex1


import org.junit.Assert.{assertEquals, assertThrows}
import org.junit.Test

class ListTest extends AbstractListTest :

  import u05lab.ex1.List._

  @Test
  def testZipRightUnordered(): Unit =
    val list: List[Int] = List(6, 3, 5, 1)
    assertEquals(List((6, 0), (3, 1), (5, 2), (1, 3)), list.zipRight)

  @Test
  def testZipRight(): Unit =
    testZipRight(list.zipRight)

  @Test
  def testPartition(): Unit =
    testPartition(list.partition)

  @Test
  def testPartition2(): Unit =
    testPartition(list.partition2)

  @Test
  def testSpan(): Unit =
    testSpan(list.span)

  @Test
  def testSpan2(): Unit =
    testSpan2(list.span)

  @Test
  def testReduce(): Unit =
    testReduce(list.reduce)

  @Test
  def testReduceError(): Unit =
    testReduceError(Nil[Int]().reduce)

  @Test
  def testTake(): Unit =
    testTake(list.takeRight)

  @Test
  def testCollect(): Unit =
    testCollect(list.collect)


