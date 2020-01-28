package org.uwplse.liquid.analysis

import heros.DefaultSeeds
import heros.FlowFunction
import heros.FlowFunctions
import heros.InterproceduralCFG
import heros.flowfunc.Identity
import heros.flowfunc.KillAll

import soot._
import soot.jimple._
import soot.jimple.internal.JimpleLocal
import soot.jimple.toolkits.ide.DefaultJimpleIFDSTabulationProblem
import soot.toolkits.scalar.Pair

import scala.collection.mutable
import scala.jdk.CollectionConverters._

class ConstantProp (val icfg: InterproceduralCFG[soot.Unit, SootMethod])
  extends
    DefaultJimpleIFDSTabulationProblem[Pair[Value, Set[Abstraction]],
    InterproceduralCFG[soot.Unit, SootMethod]](icfg) {
  type abstractionMap = mutable.Map[Unit, mutable.TreeMap[Value, Set[Abstraction]]]
  private def initAbstractionMap() = new mutable.TreeMap[soot.Unit, mutable.TreeMap[Value, Set[Abstraction]]]()(Ordering.by(_.toString()))
  val unitAbstractionMap     : abstractionMap = initAbstractionMap()
  val unitAbstractionAfterMap: abstractionMap = initAbstractionMap()
  val visitedMethods = new mutable.TreeSet[SootMethod]()(Ordering.by(_.toString()))
  private val id = Identity.v[Pair[Value, Set[Abstraction]]]()
  private val killAll = KillAll.v[Pair[Value, Set[Abstraction]]]()

  private def putUnitAbstractions(u: soot.Unit, value: Value, abstractions: Set[Abstraction]): Boolean = {
    unitAbstractionMap.getOrElseUpdate(u, new mutable.TreeMap[Value, Set[Abstraction]]()(Ordering.by(_.toString()))).addOne(value, abstractions)
    visitedMethods.add(interproceduralCFG.getMethodOf(u))
  }

  private def putUnitAbstractionsAfter(u: soot.Unit, flow: Iterable[Pair[Value, Set[Abstraction]]]): Boolean = {
    for (pair <- flow) {
      unitAbstractionAfterMap
        .getOrElseUpdate(u, new mutable.TreeMap[Value, Set[Abstraction]]()(Ordering.by(_.toString())))
        .addOne(pair.getO1, pair.getO2)
    }
    visitedMethods.add(interproceduralCFG.getMethodOf(u))
  }

  private def getInterestingConstantAbstraction(v: Value): Set[Abstraction] = {
    val constants = mutable.Set[Abstraction]()
    val values = mutable.Set(v)
    for (box <- v.getUseBoxes.asScala) {
      values.add(box.getValue)
    }

    for (value <- values) {
      value match {
        case c:StringConstant => constants.add(Abstraction.StringConstant(c.value))
        case c:IntConstant => constants.add(Abstraction.IntegerConstant(c.value))
        case _ =>
      }
    }
    constants.toSet
  }

  override def createFlowFunctionsFactory: FlowFunctions[soot.Unit, Pair[Value, Set[Abstraction]], SootMethod] = new FlowFunctions[soot.Unit, Pair[Value, Set[Abstraction]], SootMethod]() {
    override def getNormalFlowFunction(curr: soot.Unit, successor: soot.Unit): FlowFunction[Pair[Value, Set[Abstraction]]] = {
      if (curr.isInstanceOf[DefinitionStmt]) {
        val assignment = curr.asInstanceOf[DefinitionStmt]
        (source: Pair[Value, Set[Abstraction]]) => {
          def foo(source: Pair[Value, Set[Abstraction]]): Set[Pair[Value, Set[Abstraction]]] = {
            val newFlow = mutable.Set[Pair[Value, Set[Abstraction]]]()
            if (!(source == zeroValue)) {
              putUnitAbstractions(curr, source.getO1, source.getO2)
              // propagate the flow
              // (1) curr is a definition that kills previous definitions to source's LHS
              if (source.getO1.equivTo(assignment.getLeftOp)) return Set()
              // (2) doesn't kill, propagate
              if (source.getO1.equivTo(assignment.getRightOp)) return Set(new Pair[Value, Set[Abstraction]](assignment.getLeftOp, source.getO2))
              newFlow.add(source)
              putUnitAbstractionsAfter(curr, newFlow)
              newFlow.toSet
            }
            else { // generate new flow
              val constants = getInterestingConstantAbstraction(assignment.getRightOp)
              if (constants.nonEmpty) {
                Set(new Pair[Value, Set[Abstraction]](assignment.getLeftOp, constants))
              } else {
                Set()
              }
            }
          }
          foo(source).asJava
        }
      } else {
        // identity function
        id
      }
    }

    override def getCallFlowFunction(callStmt: soot.Unit, destinationMethod: SootMethod): FlowFunction[Pair[Value, Set[Abstraction]]] = {
      val stmt = callStmt.asInstanceOf[Stmt]
      val invokeExpr = stmt.getInvokeExpr
      val args = invokeExpr.getArgs
      (source: Pair[Value, Set[Abstraction]]) => {
        def foo(source: Pair[Value, Set[Abstraction]]): Set[Pair[Value, Set[Abstraction]]] = {
          val newFlow = mutable.Set[Pair[Value, Set[Abstraction]]]()
          if (destinationMethod.getName == "<clinit>" || destinationMethod.getSubSignature == "void run()") return Set(source)
          if (source ne zeroValue) {
            putUnitAbstractions(callStmt, source.getO1, source.getO2)
            // passing the definition to a local variable to the passed in the arguments in the function body
            if (args.contains(source.getO1)) {
              val paramIndex = args.indexOf(source.getO1)
              val pair = new Pair[Value, Set[Abstraction]](new EquivalentValue(Jimple.v.newParameterRef(destinationMethod.getParameterType(paramIndex), paramIndex)), source.getO2)
              newFlow.add(pair)
            }
          }
          else {
            for (arg <- args.asScala) {
              if (!arg.isInstanceOf[Local]) {
                val constants = getInterestingConstantAbstraction(arg)
                if (constants.nonEmpty) {
                  val paramIndex = args.indexOf(arg)
                  val pair = new Pair[Value, Set[Abstraction]](new EquivalentValue(Jimple.v.newParameterRef(destinationMethod.getParameterType(paramIndex), paramIndex)), constants)
                  newFlow.add(pair)
                }
              }
            }
          }
          putUnitAbstractionsAfter(callStmt, newFlow)
          newFlow.toSet
        }

        foo(source).asJava
      }
    }

    override def getReturnFlowFunction(callSite: soot.Unit, calleeMethod: SootMethod, exitStmt: soot.Unit, returnSite: soot.Unit): FlowFunction[Pair[Value, Set[Abstraction]]] = {
      if (!callSite.isInstanceOf[DefinitionStmt]) { // return value is not stored back
        return killAll
      }
      if (exitStmt.isInstanceOf[ReturnVoidStmt]) { // no return value at all
        return killAll
      }
      (source: Pair[Value, Set[Abstraction]]) => {
        def foo(source: Pair[Value, Set[Abstraction]]): Set[Pair[Value, Set[Abstraction]]]  = {
          if (source ne zeroValue) putUnitAbstractions(exitStmt, source.getO1, source.getO2)
          if (exitStmt.isInstanceOf[ReturnStmt]) {
            val returnStmt = exitStmt.asInstanceOf[ReturnStmt]
            if (returnStmt.getOp.equivTo(source.getO1)) {
              val definitionStmt = callSite.asInstanceOf[DefinitionStmt]
              val pair = new Pair[Value, Set[Abstraction]](definitionStmt.getLeftOp, source.getO2)
              putUnitAbstractionsAfter(exitStmt, Set(pair))
              return Set(pair)
            }
          }
          Set()
        }

        foo(source).asJava
      }
    }

    override def getCallToReturnFlowFunction(callSite: soot.Unit, returnSite: soot.Unit): FlowFunction[Pair[Value, Set[Abstraction]]] = { // it is just that definition might reach from things not relevant to the current call, just like
      // normal function (empty one)
      if (!(callSite.isInstanceOf[DefinitionStmt] || callSite.isInstanceOf[InvokeStmt])) return id
      val callSiteStmt = callSite.asInstanceOf[Stmt]
      val invokeExpr = callSiteStmt.getInvokeExpr
      if (invokeExpr.getMethod.getDeclaringClass.isApplicationClass) { // FIXME: Will this be a bug?
        return id
      }
      (source: Pair[Value, Set[Abstraction]]) => {
        def foo(source: Pair[Value, Set[Abstraction]]): Set[Pair[Value, Set[Abstraction]]] = {
          val newFlow = mutable.Set[Pair[Value, Set[Abstraction]]]()
          if (source ne zeroValue) {
            putUnitAbstractions(callSite, source.getO1, source.getO2)
            // Propagate to base
            if (callSite.asInstanceOf[Stmt].getInvokeExpr.isInstanceOf[InstanceInvokeExpr]) {
              val expr = callSite.asInstanceOf[Stmt].getInvokeExpr.asInstanceOf[InstanceInvokeExpr]
              newFlow.addAll(Propagator.getTainted(expr.getUseBoxes.asScala, expr.getBase, source.getO1, source.getO2))
            }
            // Propagate to the left hand side
            if (callSite.isInstanceOf[DefinitionStmt]) {
              val definitionStmt = callSite.asInstanceOf[DefinitionStmt]
              newFlow.addAll(Propagator.getTainted(definitionStmt.getUseBoxes.asScala, definitionStmt.getLeftOp, source.getO1, source.getO2))
            }
          }
          newFlow.add(source)
          putUnitAbstractionsAfter(callSite, newFlow)
          newFlow.toSet
        }

        foo(source).asJava
      }
    }
  }

  override def initialSeeds: java.util.Map[soot.Unit, java.util.Set[Pair[Value, Set[Abstraction]]]] = {
    DefaultSeeds.make(Scene.v.getEntryPoints.asScala.filter(_.hasActiveBody).map(_.getActiveBody.getUnits.getFirst).asJava, zeroValue)
  }

  override def createZeroValue = new Pair[Value, Set[Abstraction]](new JimpleLocal("<<zero>>", NullType.v), Set())
}

