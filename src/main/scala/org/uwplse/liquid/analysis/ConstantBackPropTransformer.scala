package org.uwplse.liquid.analysis

import java.io.{FileNotFoundException, PrintWriter}

import heros.InterproceduralCFG
import heros.solver.IFDSSolver
import org.uwplse.liquid.Config
import soot.jimple.{IntConstant, Stmt, StringConstant}
import soot.{SceneTransformer, SootMethod, Value}
import soot.jimple.toolkits.ide.icfg.{BackwardsInterproceduralCFG, JimpleBasedInterproceduralCFG}

import scala.collection.mutable
import scala.jdk.CollectionConverters._

class ConstantBackPropTransformer(val sink: Stmt, val config: Config) extends SceneTransformer {
  private var solver: IFDSSolver[soot.Unit, Value, SootMethod, InterproceduralCFG[soot.Unit, SootMethod]] = _
  val constantValues: mutable.Set[ConstVal] = mutable.Set[ConstVal]()

  override protected def internalTransform(phaseName: String, map: java.util.Map[String, String]): Unit = {
    val icfg = new BackwardsInterproceduralCFG(new JimpleBasedInterproceduralCFG())
    val analysis = new ConstantBackProp(icfg, sink)
    solver = new IFDSSolver[soot.Unit, Value, SootMethod, InterproceduralCFG[soot.Unit, SootMethod]](analysis)
    println(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Starting solver")
    solver.solve()
    println(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Done")

    for (m <- analysis.visitedMethods) {
      for (unit <- m.getActiveBody.getUnits.asScala) {
        val abstractions      = analysis.unitAbstractionMap.get(unit)
        val abstractionsAfter = analysis.unitAbstractionAfterMap.get(unit)
        if (abstractionsAfter.isDefined) {
          abstractionsAfter.get.foreach {
            case c: IntConstant => constantValues.add(ConstVal.IntegerConstant(c.value))
            case c: StringConstant => constantValues.add(ConstVal.StringConstant(c.value))
            case _ =>
          }
        }
        if (abstractions.isDefined) {
          abstractions.get.foreach {
            case c: IntConstant => constantValues.add(ConstVal.IntegerConstant(c.value))
            case c: StringConstant => constantValues.add(ConstVal.StringConstant(c.value))
            case _ =>
          }
        }
      }
    }

    if (config.abstractionDumpPath.isDefined) try {
      val printWriter: PrintWriter = new PrintWriter(config.abstractionDumpPath.get)
      for (m <- analysis.visitedMethods) {
        printWriter.println("====== Method " + m.getSignature + " =======")
        printWriter.println(m.getActiveBody)
        for (unit <- m.getActiveBody.getUnits.asScala) {
          val abstractions      = analysis.unitAbstractionMap.get(unit)
          val abstractionsAfter = analysis.unitAbstractionAfterMap.get(unit)
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
  }
}