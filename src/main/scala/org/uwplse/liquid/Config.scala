package org.uwplse.liquid

import java.util.Collections

import scala.beans.BeanProperty

class Config {
  @BeanProperty var whitelistPackagePrefixes: java.util.List[String] = Collections.emptyList()
  var interactive: Boolean = false
  var abstractionDumpPath: Option[String] = None
  var apkPath: String = ""
  var scored: Boolean = false

  override def toString: String = {
    s"Scored: $scored\nInteractive: $interactive"
  }
}
