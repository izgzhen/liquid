package org.uwplse.liquid

import java.util.Collections

import org.uwplse.liquid.SootInputMode.Java

import scala.beans.BeanProperty

class Config {
  @BeanProperty var whitelistPackagePrefixes: java.util.List[String] = Collections.emptyList()
  @BeanProperty var adPackagePrefixes: java.util.List[String] = Collections.emptyList()
  var interactive: Boolean = false
  var abstractionDumpPath: Option[String] = None
  var input: SootInputMode = Java("")
  var scored: Boolean = false

  override def toString: String = {
    s"Scored: $scored\nInteractive: $interactive"
  }
}
