package org.uwplse.liquid.spec

import org.uwplse.liquid.spec.Expr.{LitExpr, VarExpr}
import org.uwplse.liquid.spec.Literal.{BoolLit, IntLit}
import soot.{Local, SootMethod, Value}
import soot.jimple.{IntConstant, Stmt}
import soot.jimple.toolkits.pointer.LocalMustAliasAnalysis
import soot.toolkits.graph.CompleteUnitGraph

sealed abstract class SemanticVal extends Product with Serializable

object SemanticVal {
  final case class SootValue(v: Value, ctx: SootValueContext) extends SemanticVal
  final case class Name(name: String) extends SemanticVal
}

sealed abstract class Constraint extends Product with Serializable {
  def &&(that: Constraint) : Constraint = Constraint.and(this, that)
  def ||(that: Constraint) : Constraint = Constraint.or(this, that)
  def solve(): List[Map[String, SemanticVal]]
}

object Constraint {

  val aliasAnalysisMap: scala.collection.mutable.Map[SootMethod, LocalMustAliasAnalysis] = scala.collection.mutable.Map()

  // TODO: reason about this (https://github.com/izgzhen/liquid/issues/8)
  //   compute "and" with an empty list of constraint results in an empty list
  def solveFromBoolean(b: Boolean) : List[Map[String, SemanticVal]] = {
    if (b) { List(Map()) } else { List() }
  }

  def isAlias(l1: Local, l2: Local, stmt1: Stmt, stmt2: Stmt, m: SootMethod): Boolean = {
    if (!aliasAnalysisMap.contains(m)) {
      val ug = new CompleteUnitGraph(m.getActiveBody)
      aliasAnalysisMap.addOne(m, new LocalMustAliasAnalysis(ug, false))
    }
    val analysis = aliasAnalysisMap(m)
    analysis.mustAlias(l1, stmt1, l2, stmt2)
  }

  def equalValue(v1: SemanticVal, v2: SemanticVal): Boolean = {
    (v1, v2) match {
      case (SemanticVal.SootValue(sv1, ctx1), SemanticVal.SootValue(sv2, ctx2)) => {
        ctx2.methodEnv.sootMethod == ctx1.methodEnv.sootMethod &&
        {
          (sv1, sv2) match {
            case (l1:Local, l2: Local) =>
              isAlias(l1, l2, ctx2.stmt, ctx1.stmt, ctx1.methodEnv.sootMethod)
            case _ => throw new NotImplementedError()
          }
        }
      }
      case _ => throw new NotImplementedError()
    }
  }

  final case class NameEquals(binder: String, name: String) extends Constraint {
    def solve(): List[Map[String, SemanticVal]] = {
      List(Map((binder, SemanticVal.Name(name))))
    }
  }

  final case class ExprEquals(e: Expr, v: Value, ctx: SootValueContext) extends Constraint {
    def solve(): List[Map[String, SemanticVal]] = {
      e match {
        case VarExpr(binder) => List(Map((binder, SemanticVal.SootValue(v, ctx))))
        case LitExpr(l) => {
          (l, v) match {
            case (IntLit(i), c:IntConstant) => {
              solveFromBoolean(i == c.value)
            }
            case (BoolLit(b), c:IntConstant) => {
              if (b) { solveFromBoolean(c.value == 1) } else { solveFromBoolean(c.value == 0) }
            }
            case _ => solveFromBoolean(false)
          }
        }
        case _ => throw new NotImplementedError()
      }
    }
  }

  final case class And(c1: Constraint, c2: Constraint) extends Constraint {
    def solve(): List[Map[String, SemanticVal]] = {
      val ms1 = c1.solve()
      val ms2 = c2.solve()
      ms1.flatMap(m1 => ms2.flatMap(m2 => {
        val common = m1.keySet intersect m2.keySet
        if (common.nonEmpty && !common.forall(k => equalValue(m1(k), m2(k)))) {
          None
        } else {
          Some(m1 ++ m2)
        }
      }))
    }
  }
  final case class Or(c1: Constraint, c2: Constraint) extends Constraint {
    def solve(): List[Map[String, SemanticVal]] = {
      c1.solve() ++ c2.solve()
    }
  }
  final case class True() extends Constraint {
    def solve(): List[Map[String, SemanticVal]] = {
      solveFromBoolean(true)
    }
  }

  final case class False() extends Constraint {
    def solve(): List[Map[String, SemanticVal]] = {
      solveFromBoolean(false)
    }
  }

  def foldOr(cs: Iterable[Constraint]): Constraint = {
    if (cs.isEmpty) {
      True() // TODO: this doesn't look intuitive
    } else {
      cs.fold(False())(or)
    }
  }

  def foldAnd(cs: Iterable[Constraint]): Constraint = { cs.fold(True())(and) }

  def from(b: Boolean): Constraint = {
    if (b) { True() } else { False() }
  }

  def and(c1: Constraint, c2: Constraint): Constraint = {
    if (c1.isInstanceOf[False] || c2.isInstanceOf[False]) {
      Constraint.False()
    } else if (c1.isInstanceOf[True]) {
      c2
    } else if (c2.isInstanceOf[True]) {
      c1
    } else {
      Constraint.And(c1, c2)
    }
  }

  def or(c1: Constraint, c2: Constraint): Constraint = {
    if (c1.isInstanceOf[True] || c2.isInstanceOf[True]) {
      Constraint.True()
    } else if (c1.isInstanceOf[False]) {
      c2
    } else if (c2.isInstanceOf[False]) {
      c1
    } else {
      Constraint.Or(c1, c2)
    }
  }
}