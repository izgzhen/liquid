package org.uwplse.liquid.spec

import org.uwplse.liquid.spec.Utils._
import org.uwplse.liquid.Config
import org.uwplse.liquid.spec.IdentifierPattern.NamedWildcard
import soot.SootClass

import scala.jdk.CollectionConverters._

case class ClassSpec(name: IdentifierPattern, parent: Option[IdentifierPattern],
                     methods: List[MethodSpec]) {
  def matches(config: Config, appSpec: AppSpec, cls: SootClass, ctx: Binding) : OptBindings = {
    val filteredOut = parent match {
      case Some(NamedWildcard("packageNotWhitelisted")) =>
        config.whitelistPackagePrefixes.asScala.exists(cls.getName.startsWith)
      case _ => false
    }
    if (filteredOut) {
      return optBindings(false)
    }

    name.matches(cls.getName) match {
      case Some(nameBinding) =>
        val matchedParent = if (parent.isDefined) {
          if (cls.hasSuperclass || cls.getInterfaceCount > 0) {
            if (cls.hasSuperclass) {
              parent.get.matches(cls.getSuperclass.getName)
            } else {
              val matches = cls.getInterfaces.asScala.map(i => parent.get.matches(i.getName))
              matches.filter(_.isDefined) match {
                case Some(x)::_ => Some(x)
                case _ => None
              }
            }
          } else {
            optBinding(false)
          }
        } else {
          optBinding(true)
        }
        matchedParent match {
          case Some(parentBinding) =>
            val bs = choose(cls.getMethods.asScala.toList, methods.size).flatMap(chosen => {
              chosen.zip(methods).map({ case (m, spec) =>
                val matches = spec.matches(config, appSpec, this, m, ctx)
                matches
              }).fold(optBindings(true))(mergeOptBindings)
            }).toList.flatten
            Some(extend(extend(bs, nameBinding), parentBinding))
          case _ => None
        }
      case None => None
    }
  }
}