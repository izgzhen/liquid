package org.uwplse.liquid.spec

import org.uwplse.liquid.Config
import org.uwplse.liquid.spec.Constraint.{False, True}
import org.uwplse.liquid.spec.IdentifierPattern.NamedWildcard
import soot.SootClass

import scala.jdk.CollectionConverters._

case class ClassSpec(name: IdentifierPattern, parent: Option[IdentifierPattern],
                     methods: List[MethodSpec]) {
  def matches(config: Config, appSpec: AppSpec, cls: SootClass) : Constraint = {
    val c1 = name.matches(cls.getName)
    val filteredOut = parent match {
      case Some(NamedWildcard("packageNotWhitelisted")) =>
        config.whitelistPackagePrefixes.asScala.exists(cls.getName.startsWith)
      case _ => false
    }
    if (filteredOut) {
      return False()
    }
    val matchedParent = if (parent.isDefined) {
      if (cls.hasSuperclass || cls.getInterfaceCount > 0) {
        if (cls.hasSuperclass) {
          parent.get.matches(cls.getSuperclass.getName)
        } else {
          Constraint.foldOr(cls.getInterfaces.asScala.map(i => parent.get.matches(i.getName)))
        }
      } else {
        False()
      }
    } else {
      True()
    }
    c1 && matchedParent && Constraint.foldAnd(methods.map(mSpec =>
      Constraint.foldOr(cls.getMethods.asScala.map(mSpec.matches(appSpec, this, _)))))
  }
}