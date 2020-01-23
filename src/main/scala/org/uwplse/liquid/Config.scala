package org.uwplse.liquid

import java.util.Collections

import scala.beans.BeanProperty

class Config {
  @BeanProperty var whitelistPackagePrefixes: java.util.List[String] = Collections.emptyList()
  var interactive: Boolean = false
}
