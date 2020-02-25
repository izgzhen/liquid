package org.uwplse.liquid

import java.io.{FileNotFoundException, PrintWriter}
import java.util.Collections

import org.uwplse.liquid.SootInputMode.{Android, Java}
import org.uwplse.liquid.analysis.{ConstantVal, DependencyBackProp, DependencyForwardProp, DependencyProp, Measure}
import org.uwplse.liquid.spec.{ConcreteVal, IdentifierPattern}
import heros.InterproceduralCFG
import heros.solver.IFDSSolver
import soot.jimple.infoflow.entryPointCreators.DefaultEntryPointCreator
import soot.{Local, RefType, Scene, SootClass, SootMethod, Type, Value}
import soot.jimple.{IntConstant, InvokeStmt, Stmt, StringConstant}
import soot.jimple.toolkits.callgraph.ReachableMethods
import soot.jimple.toolkits.ide.icfg.{BackwardsInterproceduralCFG, JimpleBasedInterproceduralCFG}
import soot.jimple.toolkits.pointer.LocalMustAliasAnalysis
import soot.options.Options
import soot.toolkits.graph.CompleteUnitGraph
import soot.toolkits.scalar.{SimpleLiveLocals, SmartLocalDefs}
import org.uwplse.liquid.analysis.SootUtils._

import scala.collection.mutable
import scala.jdk.CollectionConverters._

object Analysis {
  var config: Option[Config] = None

  private val reachableMethodsMap = mutable.Map[SootMethod, ReachableMethods]()
  private val reachableMap = mutable.Map[SootMethod, mutable.Set[SootMethod]]()
  private val aliasAnalysisMap = mutable.Map[SootMethod, LocalMustAliasAnalysis]()
  private val localDefsMap = mutable.Map[SootMethod, SmartLocalDefs]()
  private val constantsMap = mutable.Map[Stmt, Set[Value]]()
  private var allClasses: Set[SootClass] = _
  private var allMethods: Set[SootMethod] = _

  /**
   * https://soot-build.cs.uni-paderborn.de/public/origin/develop/soot/soot-develop/options/soot_options.htm
   * @param mode
   */
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
    Options.v.set_exclude(List("java.*", "com.google.*", "android.support.*").asJava)
    Options.v.set_prepend_classpath(true)
    Options.v.set_whole_program(true)
    Options.v.set_allow_phantom_refs(true)
    Options.v.set_ignore_resolution_errors(true)
    Options.v.set_no_writeout_body_releasing(true)
    Options.v.set_output_format(Options.output_format_none)
    Options.v.setPhaseOption("cg.spark", "enabled:true")
    Options.v.set_no_bodies_for_excluded(true)
    Options.v.set_omit_excepting_unit_edges(true)
  }

  private def setEntrypoints(): Unit = {
    var entrypoints = List[SootMethod]()
    var classes = List[SootClass]()

    Scene.v().getApplicationClasses.forEach((appCls: SootClass) => {
      if (appCls.isConcrete) {
        Scene.v.forceResolve(appCls.getName, SootClass.BODIES)
        classes :+= appCls
        appCls.getMethods.forEach((appMethod: SootMethod) => {
          try {
            appMethod.retrieveActiveBody
            val b = appMethod.getActiveBody
            if (b != null) {
              entrypoints :+= appMethod
            }
          } catch {
            case _: RuntimeException =>
          }
        })
      }
    })

    val entryPointCreator = new DefaultEntryPointCreator(entrypoints.map(_.getSignature).asJava)
    val dummyMain = entryPointCreator.createDummyMain
    Scene.v.setEntryPoints(Collections.singletonList(dummyMain))

    allClasses = classes.toSet
    println(s"allClasses: ${allClasses.size}")
    val allPkgs = allClasses.toList.map(_.getPackageName.split("\\.").slice(0, 2).mkString("."))
    val counted = allPkgs.toSet.map((pkg: String) => (pkg, allPkgs.count(p => p == pkg)))
    for ((pkg, count) <- counted) {
      println(s"- ${pkg}: ${count}")
    }

    allMethods = Scene.v().getClasses.asScala.flatMap(_.getMethods.asScala).toSet
    println(s"allMethods: ${allMethods.size}")
  }

  /**
   * Setup the analysis singleton
   * @param config
   * @return
   */
  def setup(config: Config): Unit = {
    this.config = Some(config)
    setSootOptions(config.input)
    soot.Scene.v.loadNecessaryClasses()
    setEntrypoints()
  }

  def getAllAppClasses: Set[SootClass] = allClasses
  def getAllMethods: Set[SootMethod] = allMethods

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

  def reachableUpdate(currentMethod: SootMethod, entryPoint: SootMethod): Unit = {
    val workList = mutable.Stack(currentMethod)
    while (workList.nonEmpty) {
      val m = workList.pop()
      if (!reachableMap(entryPoint).contains(m)) {
        reachableMap(entryPoint).addOne(m)
        if (m.hasActiveBody) {
          m.getActiveBody.getUnits.forEach {
            case invokeStmt: InvokeStmt => workList.push(invokeStmt.getInvokeExpr.getMethod)
            case _ =>
          }
        }
      }
    }
  }

  def isReachable(m1: SootMethod, m2: SootMethod): Boolean = {
    if (!reachableMethodsMap.contains(m1)) {
      val cg = soot.Scene.v.getCallGraph
      val rm = new ReachableMethods(cg, List(m1).asJava)
      rm.update()
      reachableMethodsMap.addOne(m1, rm)
    }
    if (reachableMethodsMap(m1).contains(m2)) {
      return true
    }
    if (!reachableMap.contains(m1)) {
      reachableMap.addOne(m1, mutable.Set[SootMethod]())
      reachableUpdate(m1, m1)
    }
    reachableMap(m1).contains(m2)
  }

  private var solver: IFDSSolver[soot.Unit, (Value, Set[ConstantVal]), SootMethod, InterproceduralCFG[soot.Unit, SootMethod]] = null

  def dependencyPropAnalysis(sinkMethod: SootMethod, sink: Stmt, abstractionDumpPath: Option[String], recordAbstractions: Boolean): Set[Value] = {
    if (solver == null) {
      val icfg = new JimpleBasedInterproceduralCFG()
      val analysis = new DependencyProp(icfg, recordAbstractions)
      solver = new IFDSSolver(analysis)
      System.out.println("========================  Solver started  ========================")
      solver.solve()
      System.out.println("========================  Solver finished ========================")
      if (abstractionDumpPath.isDefined && recordAbstractions) try {
        val printWriter: PrintWriter = new PrintWriter(abstractionDumpPath.get)
        for (m <- analysis.visitedMethods) {
          printWriter.println("====== Method " + m.getSignature + " =======")
          printWriter.println(m.getActiveBody)
          for (unit <- m.getActiveBody.getUnits.asScala) {
            val abstractions = analysis.unitAbstractionMap.get(unit)
            if (abstractions.isDefined) {
              for (value <- abstractions.get) {
                for (abstraction <- value._2) {
                  printWriter.println("\t\t" + value._1 + ": " + abstraction)
                }
              }
            }
            if (abstractions.isDefined && abstractions.get.nonEmpty) printWriter.println("\tUnit: " + unit)
            printWriter.println()
          }
        }
        printWriter.close()
      } catch {
        case e: FileNotFoundException =>
          println(e.toString)
      }
    }
    val (locals, constants) = getUsedVals(sink)
    val allConstants = solver.ifdsResultsAt(sink).asScala.collect {
      case (v:Local, cs) if locals.exists(l => isAlias(l, v, sink, sink, sinkMethod)) => cs
    }.flatten.toSet ++ constants
    allConstants.map {
      case ConstantVal.IntConst(i) => IntConstant.v(i)
      case ConstantVal.StrConst(s) => StringConstant.v(s)
    }
  }

  private val backwardSolverMeasure = new Measure("backwardSolver.solve")
  private val forwardSolverMeasure  = new Measure("forwardSolver.solve")

  /**
   * This one might be slower since it could invoke a lot of IFDS analyses
   * @param sink
   * @param abstractionDumpPath
   * @return
   */
  def dependencyPropAnalysis2(sinkMethod: SootMethod, sink: Stmt, abstractionDumpPath: Option[String]): Set[Value] = {
    val vals = mutable.Set[Value]()
    val icfg = new JimpleBasedInterproceduralCFG(false)
    if (icfg.getMethodOf(sink) == null) {
      return Set()
    }
    val backIcfg = new BackwardsInterproceduralCFG(icfg)
    if (backIcfg.getMethodOf(sink) == null) {
      return Set()
    }
    val backwardAnalysis = new DependencyBackProp(backIcfg, sink)
    val backwardSolver = new IFDSSolver[soot.Unit, Value, SootMethod, InterproceduralCFG[soot.Unit, SootMethod]](backwardAnalysis)
    backwardSolverMeasure.incr(backwardSolver.solve)

    val forwardAnalysis = new DependencyForwardProp(icfg, backwardAnalysis.unitAbstractionAfterMap, sink)
    val forwardSolver = new IFDSSolver[soot.Unit, Value, SootMethod, InterproceduralCFG[soot.Unit, SootMethod]](forwardAnalysis)
    forwardSolverMeasure.incr(forwardSolver.solve)
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

  def getConstantFlowIns(sinkMethod: SootMethod, sink: Stmt): Set[Value] = {
    if (!constantsMap.contains(sink)) {
      constantsMap.addOne(sink, dependencyPropAnalysis2(sinkMethod, sink, getConfig.abstractionDumpPath))
    }
    constantsMap(sink)
  }

  def isAlias(l1: Local, l2: Local, stmt1: Stmt, stmt2: Stmt, m: SootMethod): Boolean = {
    if (l1.equivTo(l2)) {
      return true
    }
    if (!aliasAnalysisMap.contains(m)) {
      val ug = new CompleteUnitGraph(m.getActiveBody)
      aliasAnalysisMap.addOne(m, new LocalMustAliasAnalysis(ug, false))
    }
    val analysis = aliasAnalysisMap(m)
    analysis.mustAlias(l1, stmt1, l2, stmt2) || getLocalDefs(m).getDefsOf(l2) == getLocalDefs(m).getDefsOf(l1)
  }

  def equalValue(v1: ConcreteVal, v2: ConcreteVal): Boolean = {
    (v1, v2) match {
      case (ConcreteVal.SootValue(sv1, ctx1), ConcreteVal.SootValue(sv2, ctx2)) => {
        ctx2.methodEnv.sootMethod == ctx1.methodEnv.sootMethod &&
          {
            (sv1, sv2) match {
              case (l1:Local, l2: Local) =>
                isAlias(l1, l2, ctx1.stmt, ctx2.stmt, ctx1.methodEnv.sootMethod)
              case _ => throw new NotImplementedError()
            }
          }
      }
      case (ConcreteVal.Name(x), ConcreteVal.Name(y)) => x == y
      case (ConcreteVal.Method(f1), ConcreteVal.Method(f2)) => f1 == f2
      case _ => throw new NotImplementedError()
    }
  }

  def booleanEqualsInt(b: Boolean, i: Int) : Boolean = {
    if (b) { i == 1 } else { i == 0 }
  }

  def getConfig: Config = config.get

  def typeMatch(typeSpec: String, inputType: Type): Boolean = {
    inputType match {
      case ref:RefType =>
        inputType.toString == typeSpec || isSubClass(ref.getSootClass, IdentifierPattern.StringIdentifier(typeSpec))
      case _ =>
        inputType.toString == typeSpec
    }
  }

  def isSubClass(child: SootClass, parent: IdentifierPattern): Boolean = {
    if (child.hasSuperclass || child.getInterfaceCount > 0) {
      (child.hasSuperclass && parent.matches(child.getSuperclass.getName).isDefined) ||
        child.getInterfaces.asScala.map(i => parent.matches(i.getName)).exists(_.isDefined)
    } else {
      false
    }
  }
}