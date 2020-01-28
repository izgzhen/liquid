package org.uwplse.liquid.analysis

import java.io.{FileNotFoundException, PrintWriter}

import heros.InterproceduralCFG
import heros.solver.IFDSSolver
import org.uwplse.liquid.Config
import soot.{SceneTransformer, SootMethod, Value}
import soot.jimple.toolkits.ide.icfg.JimpleBasedInterproceduralCFG
import soot.toolkits.scalar.Pair

import scala.jdk.CollectionConverters._

class ConstantPropTransformer(val config: Config) extends SceneTransformer {
  private var solver: IFDSSolver[soot.Unit, Pair[Value, Set[ConstVal]], SootMethod, InterproceduralCFG[soot.Unit, SootMethod]] = _

  override protected def internalTransform(phaseName: String, map: java.util.Map[String, String]): Unit = {
    val icfg = new JimpleBasedInterproceduralCFG()
    val analysis = new ConstantProp(icfg)
    solver = new IFDSSolver[soot.Unit, Pair[Value, Set[ConstVal]], SootMethod, InterproceduralCFG[soot.Unit, SootMethod]](analysis)
    println(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Starting solver")
    solver.solve()
    println(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Done")
    if (config.abstractionDumpPath.isDefined) try {
      val printWriter: PrintWriter = new PrintWriter(config.abstractionDumpPath.get)
      for (m <- analysis.visitedMethods) {
        printWriter.println("====== Method " + m.getSignature + " =======")
        printWriter.println(m.getActiveBody)
        for (unit <- m.getActiveBody.getUnits.asScala) {
          val abstractions      = analysis.unitAbstractionMap.get(unit)
          val abstractionsAfter = analysis.unitAbstractionAfterMap.get(unit)
          if (abstractions.isDefined) {
            for (value <- abstractions.get) {
              for (abstraction <- value._2) {
                printWriter.println("\t\t" + value._1 + ": " + abstraction)
              }
            }
          }
          if ((abstractions.isDefined && abstractions.get.nonEmpty) || (abstractionsAfter.isDefined && abstractionsAfter.get.nonEmpty)) printWriter.println("\tUnit: " + unit)
          if (abstractionsAfter.isDefined) {
            for (value <- abstractionsAfter.get) {
              for (abstraction <- value._2) {
                printWriter.println("\t\t" + value._1 + ": " + abstraction)
              }
            }
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

  def getSolver: IFDSSolver[soot.Unit, Pair[Value, Set[ConstVal]], SootMethod, InterproceduralCFG[soot.Unit, SootMethod]] = solver
}
