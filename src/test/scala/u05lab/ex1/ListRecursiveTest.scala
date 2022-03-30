package u05lab.ex1

import org.junit.Assert.{assertEquals, assertThrows}
import org.junit.Test

class ListRecursiveTest {

    import u05lab.ex1.List.*

    val list: List[Int] = List(1, 2, 3, 4)

    @Test
    def testZipRightRec(): Unit =
      assertEquals(List((1, 0), (2, 1), (3, 2), (4, 3)), list.zipRightRec)

    @Test
    def testPartitionRec(): Unit =
      assertEquals((List(1, 3), List(2, 4)), list.partitionRec(_ % 2 == 1))

    @Test
    def testSpanRec(): Unit =
      assertEquals((List(1), List(2, 3, 4)), list.spanRec(_ % 2 != 0))

    @Test
    def testSpanRec2(): Unit =
      assertEquals((List(1, 2), List(3, 4)), list.spanRec(_ < 3))

    @Test
    def testReduceRec(): Unit =
      assertEquals(10, list.reduceRec(_ + _))

    @Test
    def testReduceRecError(): Unit =
      assertThrows(classOf[UnsupportedOperationException], () => Nil[Int]().reduceRec(_ + _))

    @Test
    def testTakeRec(): Unit =
      assertEquals(List(2, 3, 4), list.takeRightRec(3))

    @Test
    def testCollect(): Unit =
      val fun = new PartialFunction[Int, Int] {
        def isDefinedAt(e: Int): Boolean = e % 2 == 0

        def apply(e: Int): Int = e * 2

      }
      assertEquals(List(4, 8), list.collectRec(fun))

  }
