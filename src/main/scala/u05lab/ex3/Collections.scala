package u05lab.ex3

import u05lab.ex3.PerformanceUtils.measure

import java.util.concurrent.TimeUnit
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.concurrent.duration.FiniteDuration

object PerformanceUtils:
  case class MeasurementResults[T](result: T, duration: FiniteDuration) extends Ordered[MeasurementResults[_]] :
    override def compare(that: MeasurementResults[_]): Int = duration.toNanos.compareTo(that.duration.toNanos)

  def measure[T](msg: String)(expr: => T): MeasurementResults[T] =
    val startTime = System.nanoTime()
    val res = expr
    val duration = FiniteDuration(System.nanoTime() - startTime, TimeUnit.NANOSECONDS)
    if (msg.nonEmpty) println(msg + " -- " + duration.toNanos + " nanos; " + duration.toMillis + "ms")
    MeasurementResults(res, duration)

  def measure[T](expr: => T): MeasurementResults[T] = measure("")(expr)

@main def checkPerformance: Unit =

  /* Linear sequences: List, ListBuffer */
  val list = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  val listBuffer = ListBuffer(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  println(measure("List: create")(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)))
  println(measure("ListBuffer: create")(ListBuffer(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)))
  println(measure("List: access")(list(5)))
  println(measure("ListBuffer: access")(listBuffer(5)))
  println(measure("List: size")(list.size))
  println(measure("ListBuffer: size")(listBuffer.size))
  println(measure("List: append")(list :+ 11))
  println(measure("ListBuffer: append")(listBuffer += 11))
  println(measure("ListBuffer: update")(listBuffer(5) = 11))
  println(measure("ListBuffer: remove")(listBuffer -= 5))
  println("-----------------------------------------------------")

  /* Indexed sequences: Vector, Array, ArrayBuffer */
  val vector = Vector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  val array = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  val arrayBuffer = ArrayBuffer(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  println(measure("Vector: create")(Vector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)))
  println(measure("Array: create")(Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)))
  println(measure("ArrayBuffer: create")(ArrayBuffer(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)))
  println(measure("Vector: access")(vector(5)))
  println(measure("Array: access")(array(5)))
  println(measure("ArrayBuffer: access")(arrayBuffer(5)))
  println(measure("Vector: size")(vector.size))
  println(measure("Array: size")(array.length))
  println(measure("ArrayBuffer: size")(arrayBuffer.size))
  println(measure("Vector: append")(vector :+ 11))
  println(measure("Array: append")(array :+ 11))
  println(measure("ArrayBuffer: append")(arrayBuffer += 11))
  println(measure("ArrayBuffer: update")(arrayBuffer(5) = 11))
  println(measure("ArrayBuffer: remove")(arrayBuffer -= 5))
  println("-----------------------------------------------------")

  /* Sets */
  val set = Set(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  val mutableSet = scala.collection.mutable.Set(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  println(measure("Set: create")(Set(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)))
  println(measure("MutableSet: create")(scala.collection.mutable.Set(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)))
  println(measure("Set: access")(set(5)))
  println(measure("MutableSet: access")(mutableSet(5)))
  println(measure("Set: size")(set.size))
  println(measure("MutableSet: size")(mutableSet.size))
  println(measure("Set: append")(set + 11))
  println(measure("MutableSet: append")(mutableSet += 11))
  println(measure("Set: remove")(set - 5))
  println(measure("MutableSet: remove")(mutableSet -= 5))
  println("-----------------------------------------------------")

  /* Maps */
  val map = Map(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four", 5 -> "five", 6 -> "six", 7 -> "seven", 8 -> "eight", 9 -> "nine", 10 -> "ten")
  val mutableMap = scala.collection.mutable.Map(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four", 5 -> "five", 6 -> "six", 7 -> "seven", 8 -> "eight", 9 -> "nine", 10 -> "ten")
  println(measure("Map: create")(Map(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four", 5 -> "five", 6 -> "six", 7 -> "seven", 8 -> "eight", 9 -> "nine", 10 -> "ten")))
  println(measure("MutableMap: create")(scala.collection.mutable.Map(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four", 5 -> "five", 6 -> "six", 7 -> "seven", 8 -> "eight", 9 -> "nine", 10 -> "ten")))
  println(measure("Map: access")(map(5)))
  println(measure("MutableMap: access")(mutableMap(5)))
  println(measure("Map: size")(map.size))
  println(measure("MutableMap: size")(mutableMap.size))
  println(measure("Map: append")(map + (11 -> "eleven")))
  println(measure("MutableMap: append")(mutableMap += (11 -> "eleven")))
  println(measure("Map: remove")(map - 5))
  println(measure("MutableMap: remove")(mutableMap -= 5))
  println("-----------------------------------------------------")

  /* Comparison */
  import PerformanceUtils.*
  val lst = (1 to 10000000).toList
  val vec = (1 to 10000000).toVector
  assert(measure("lst last")(lst.last) > measure("vec last")(vec.last))
