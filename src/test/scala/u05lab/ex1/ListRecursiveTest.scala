package u05lab.ex1

import org.junit.Assert.{assertEquals, assertThrows}
import org.junit.Test

class ListRecursiveTest extends AbstractListTest :

  import u05lab.ex1.List._

  @Test
  def testZipRight(): Unit =
    testZipRight(list.zipRightRec)

  @Test
  def testPartition(): Unit =
    testPartition(list.partitionRec)

  @Test
  def testSpan(): Unit =
    testSpan(list.spanRec)

  @Test
  def testSpan2(): Unit =
    testSpan2(list.spanRec)

  @Test
  def testReduce(): Unit =
    testReduce(list.reduceRec)

  @Test
  def testReduceError(): Unit =
    testReduceError(Nil[Int]().reduceRec)

  @Test
  def testTake(): Unit =
    testTake(list.takeRightRec)

  @Test
  def testCollect(): Unit =
    testCollect(list.collectRec)