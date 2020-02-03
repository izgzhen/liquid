package org.uwplse.liquid

import java.io.{FileNotFoundException, PrintWriter}
import java.util.Collections

import heros.InterproceduralCFG
import heros.solver.IFDSSolver
import org.uwplse.liquid.SootInputMode.{Android, Java}
import org.uwplse.liquid.analysis.{DependencyBackProp, DependencyForwardProp}
import org.uwplse.liquid.spec.SemanticVal
import soot.{Local, Scene, SootClass, SootMethod, Value}
import soot.jimple.{IntConstant, Stmt, StringConstant}
import soot.jimple.infoflow.entryPointCreators.DefaultEntryPointCreator
import soot.jimple.toolkits.callgraph.ReachableMethods
import soot.jimple.toolkits.ide.icfg.{BackwardsInterproceduralCFG, JimpleBasedInterproceduralCFG}
import soot.jimple.toolkits.pointer.LocalMustAliasAnalysis
import soot.options.Options
import soot.toolkits.graph.CompleteUnitGraph
import soot.toolkits.scalar.{SimpleLiveLocals, SmartLocalDefs}

import scala.collection.mutable
import scala.jdk.CollectionConverters._

object Analysis {
  private val reachableMethodsMap = mutable.Map[SootMethod, ReachableMethods]()
  private val aliasAnalysisMap = mutable.Map[SootMethod, LocalMustAliasAnalysis]()
  private val localDefsMap = mutable.Map[SootMethod, SmartLocalDefs]()
  private val constantsMap = mutable.Map[Stmt, Set[Value]]()

  def setSootOptions(mode: SootInputMode) : Unit = {
    soot.G.reset()
    mode match {
      case Android(apkPath) =>
        Options.v.set_process_dir(Collections.singletonList(apkPath))
        Options.v.set_android_jars("android-platforms/")
        Options.v.set_soot_classpath("android-platforms/android-28/android.jar")
        Options.v.set_src_prec(Options.src_prec_apk)
        Options.v.set_process_multiple_dex(true)
      case Java(classPath) =>
        Options.v.set_process_dir(Collections.singletonList(classPath))
        Options.v.set_soot_classpath(classPath)
        Options.v.set_src_prec(Options.src_prec_class)
        Options.v.set_keep_line_number(true)
    }
    Options.v.set_exclude(Collections.singletonList("java.*"))
    Options.v.set_prepend_classpath(true)
    Options.v.set_whole_program(true)
    Options.v.set_allow_phantom_refs(true)
    Options.v.set_ignore_resolution_errors(true)
    Options.v.set_no_writeout_body_releasing(true)
    Options.v.set_output_format(Options.output_format_none)
    Options.v.setPhaseOption("cg.spark", "on")
    Options.v.set_no_bodies_for_excluded(true)
    Options.v.set_omit_excepting_unit_edges(true)
  }

  private def setEntrypoints(): List[SootClass] = {
    var allMethods = List[SootMethod]()
    var allClasses = List[SootClass]()

    Scene.v().getApplicationClasses.forEach((appCls: SootClass) => {
      if (appCls.isConcrete) {
        Scene.v.forceResolve(appCls.getName, SootClass.BODIES)
        allClasses :+= appCls
        appCls.getMethods.forEach((appMethod: SootMethod) => {
          try {
            appMethod.retrieveActiveBody
            val b = appMethod.getActiveBody
            if (b != null) {
              allMethods :+= appMethod
            }
          } catch {
            case _: RuntimeException =>
          }
        })
      }
    })

    val entryPointCreator = new DefaultEntryPointCreator(allMethods.map(_.getSignature).asJava)
    val dummyMain = Some(entryPointCreator.createDummyMain)
    Scene.v.setEntryPoints(Collections.singletonList(dummyMain.get))
    allClasses
  }

  def setup(config: Config): List[SootClass] = {
    setSootOptions(Android(config.apkPath))
    soot.Scene.v.loadNecessaryClasses()
    setEntrypoints()
  }

  def debugUnitToOwner(m: java.util.HashMap[Unit, soot.Body]) : Set[String] = {
    m.asScala.values.map(_.getMethod.getDeclaringClass.getName).toSet
  }

  def getLocalDefs(m: SootMethod) : SmartLocalDefs = {
    if (!localDefsMap.contains(m)) {
      val ug = new CompleteUnitGraph(m.getActiveBody)
      localDefsMap.addOne(m, new SmartLocalDefs(ug, new SimpleLiveLocals(ug)))
    }
    localDefsMap(m)
  }

  def isReachable(m1: SootMethod, m2: SootMethod): Boolean = {
    if (!reachableMethodsMap.contains(m1)) {
      val cg = soot.Scene.v.getCallGraph
      val rm = new ReachableMethods(cg, List(m1).asJava)
      rm.update()
      reachableMethodsMap.addOne(m1, rm)
    }
    reachableMethodsMap(m1).contains(m2)
  }

  def dependencyPropAnalysis(sink: Stmt, abstractionDumpPath: Option[String]): Set[Value] = {
    val vals = mutable.Set[Value]()
    val icfg = new JimpleBasedInterproceduralCFG(false)
    assert(icfg.getMethodOf(sink) != null)
    val backIcfg = new BackwardsInterproceduralCFG(icfg)
    assert(backIcfg.getMethodOf(sink) != null)
    val backwardAnalysis = new DependencyBackProp(backIcfg, sink)
    val backwardSolver = new IFDSSolver[soot.Unit, Value, SootMethod, InterproceduralCFG[soot.Unit, SootMethod]](backwardAnalysis)
    backwardSolver.solve()

    val forwardAnalysis = new DependencyForwardProp(icfg, backwardAnalysis.unitAbstractionAfterMap, sink)
    val forwardSolver = new IFDSSolver[soot.Unit, Value, SootMethod, InterproceduralCFG[soot.Unit, SootMethod]](forwardAnalysis)
    forwardSolver.solve()
    // FIXME: iterative

    for (m <- forwardAnalysis.visitedMethods) {
      for (unit <- m.getActiveBody.getUnits.asScala) {
        val abstractions      = forwardAnalysis.unitAbstractionMap.get(unit)
        val abstractionsAfter = forwardAnalysis.unitAbstractionAfterMap.get(unit)
        if (abstractionsAfter.isDefined) {
          abstractionsAfter.get.foreach {
            case c: IntConstant => vals.add(c)
            case c: StringConstant => vals.add(c)
            case _ =>
          }
        }
        if (abstractions.isDefined) {
          abstractions.get.foreach {
            case c: IntConstant => vals.add(c)
            case c: StringConstant => vals.add(c)
            case _ =>
          }
        }
      }
    }

    if (abstractionDumpPath.isDefined) try {
      val printWriter: PrintWriter = new PrintWriter(abstractionDumpPath.get)
      for (m <- forwardAnalysis.visitedMethods) {
        printWriter.println("====== Method " + m.getSignature + " =======")
        printWriter.println(m.getActiveBody)
        for (unit <- m.getActiveBody.getUnits.asScala) {
          val abstractions      = forwardAnalysis.unitAbstractionMap.get(unit)
          val abstractionsAfter = forwardAnalysis.unitAbstractionAfterMap.get(unit)
          if (abstractionsAfter.isDefined) {
            printWriter.println("\t\t" + abstractionsAfter.get)
          }
          if ((abstractions.isDefined && abstractions.get.nonEmpty) || (abstractionsAfter.isDefined && abstractionsAfter.get.nonEmpty)) printWriter.println("\tUnit: " + unit)
          if (abstractions.isDefined) {
            printWriter.println("\t\t" + abstractions.get)
          }
          printWriter.println()
        }
      }
      printWriter.close()
    } catch {
      case e: FileNotFoundException =>
        println(e.toString)
    }

    vals.toSet
  }

  def getConstantFlowIns(sink: Stmt, config: Config): Set[Value] = {
    if (!constantsMap.contains(sink)) {
      constantsMap.addOne(sink, dependencyPropAnalysis(sink, config.abstractionDumpPath))
    }
    constantsMap(sink)
  }

  def isAlias(l1: Local, l2: Local, stmt1: Stmt, stmt2: Stmt, m: SootMethod): Boolean = {
    if (!aliasAnalysisMap.contains(m)) {
      val ug = new CompleteUnitGraph(m.getActiveBody)
      aliasAnalysisMap.addOne(m, new LocalMustAliasAnalysis(ug, false))
    }
    val analysis = aliasAnalysisMap(m)
    analysis.mustAlias(l1, stmt1, l2, stmt2) || getLocalDefs(m).getDefsOf(l2) == getLocalDefs(m).getDefsOf(l1)
  }

  def equalValue(v1: SemanticVal, v2: SemanticVal): Boolean = {
    (v1, v2) match {
      case (SemanticVal.SootValue(sv1, ctx1), SemanticVal.SootValue(sv2, ctx2)) => {
        ctx2.methodEnv.sootMethod == ctx1.methodEnv.sootMethod &&
          {
            (sv1, sv2) match {
              case (l1:Local, l2: Local) =>
                isAlias(l1, l2, ctx1.stmt, ctx2.stmt, ctx1.methodEnv.sootMethod)
              case _ => throw new NotImplementedError()
            }
          }
      }
      case (SemanticVal.Name(x), SemanticVal.Name(y)) => x == y
      case (SemanticVal.Method(f1), SemanticVal.Method(f2)) => f1 == f2
      case _ => throw new NotImplementedError()
    }
  }

  def booleanEqualsInt(b: Boolean, i: Int) : Boolean = {
    if (b) { i == 1 } else { i == 0 }
  }
}