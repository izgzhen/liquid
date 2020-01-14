package org.uwplse.liquid.spec

import org.uwplse.liquid.spec.Expr.VarExpr
import soot.Value

sealed abstract class SemanticVal extends Product with Serializable

object SemanticVal {
  final case class SootValue(v: Value) extends SemanticVal
  final case class Name(name: String) extends SemanticVal
}

sealed abstract class Constraint extends Product with Serializable {
  def &&(that: Constraint) : Constraint = Constraint.and(this, that)
  def ||(that: Constraint) : Constraint = Constraint.or(this, that)
  def solve(): List[Map[String, SemanticVal]]
}

object Constraint {
  final case class NameEquals(binder: String, name: String) extends Constraint {
    def solve(): List[Map[String, SemanticVal]] = {
      List(Map((binder, SemanticVal.Name(name))))
    }
  }

  final case class ExprEquals(e: Expr, v: Value) extends Constraint {
    def solve(): List[Map[String, SemanticVal]] = {
      e match {
        case VarExpr(binder) => List(Map((binder, SemanticVal.SootValue(v))))
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
        if (common.nonEmpty && !common.forall(k => m1(k) == m2(k))) {
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
      List(Map())
    }
  }

  final case class False() extends Constraint {
    def solve(): List[Map[String, SemanticVal]] = {
      List()
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