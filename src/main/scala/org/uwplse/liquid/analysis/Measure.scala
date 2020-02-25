package org.uwplse.liquid.analysis

import scala.collection.mutable

/* Created at 2/25/20 by zhen */
class Measure(val id: String) {
  private val statsInSeconds = mutable.ListBuffer[Double]()
  def incr[T](f: () => T): T = {
    val now = System.nanoTime()
    val ret = f()
    val spent = (System.nanoTime() - now) / Math.pow(10, 9)
    statsInSeconds.append(spent)
//    if (spent > 0.1) {
//      System.out.println(f"Measure $id: $spent")
//    }
    ret
  }

  def getStats: Map[String, Double] = Map(
    id + ".avgSeconds" -> statsInSeconds.sum / statsInSeconds.size,
    id + ".count" -> statsInSeconds.size
  )
}
