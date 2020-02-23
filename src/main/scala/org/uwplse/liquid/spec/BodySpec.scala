package org.uwplse.liquid.spec

import org.uwplse.liquid.analysis.{Binding, Bindings}
import soot.Body
import soot.jimple.Stmt

import scala.jdk.CollectionConverters._

/* Created at 2/23/20 by zhen */

sealed abstract class BodySpec extends Product with Serializable {
  def solveCost(ctx: Set[String]): Long
  def matches(appSpec: AppSpec, classSpec: ClassSpec, env: MethodEnv, body: Body, ctx: Binding): Bindings
}

/**
 * There are many performance issue in matching and soundness issue in estimating the cost
 */
object BodySpec {
  final case class Just(stmts: List[StatementSpec]) extends BodySpec {
    override def solveCost(ctx: Set[String]): Long = {
      stmts.size.toLong * stmts.size.toLong
    }

    def matches(appSpec: AppSpec, classSpec: ClassSpec, env: MethodEnv, body: Body, ctx: Binding): Bindings = {
      if (stmts.isEmpty) {
        Bindings.one()
      } else {
        stmts.map(spec =>
          Bindings.from(body.getUnits.asScala.flatMap(s =>
            spec.matches(appSpec, classSpec, env, s.asInstanceOf[Stmt], ctx)
          ))
        ).fold(Bindings.one()) { case (x, y) => x.prod(y) }
      }
    }
  }

  final case class Or(s1: BodySpec, s2: BodySpec) extends BodySpec {
    override def solveCost(ctx: Set[String]): Long = {
      s1.solveCost(ctx) + s2.solveCost(ctx)
    }

    def matches(appSpec: AppSpec, classSpec: ClassSpec, env: MethodEnv, body: Body, ctx: Binding): Bindings = {
      s1.matches(appSpec, classSpec, env, body, ctx).sum(s2.matches(appSpec, classSpec, env, body, ctx))
    }
  }
}