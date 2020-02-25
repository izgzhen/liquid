package org.uwplse.liquid.analysis

import soot.{Local, Value}
import soot.jimple.{IntConstant, Stmt, StringConstant}
import scala.jdk.CollectionConverters._

/* Created at 2/24/20 by zhen */
object SootUtils {
  def getUsedConstantVals(v: Value): Set[ConstantVal] = {
    val vs = v::v.getUseBoxes.asScala.map(_.getValue).toList
    vs.collect {
      case i: IntConstant => ConstantVal.IntConst(i.value)
      case s: StringConstant => ConstantVal.StrConst(s.value)
    }.toSet
  }

  def getUsedVals(s: Stmt): (Set[Local], Set[ConstantVal]) = {
    val vals = s.getUseBoxes.asScala.map(_.getValue)
    (vals.collect { case x: Local => x }.toSet,
      vals.collect {
        case i: IntConstant => ConstantVal.IntConst(i.value)
        case s: StringConstant => ConstantVal.StrConst(s.value)
      }.toSet)
  }
}
