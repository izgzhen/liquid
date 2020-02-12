package org.uwplse.liquid

import java.io.FileInputStream
import org.apache.commons.cli.{DefaultParser, Option, Options}
import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor

object Main {
  def main(args: Array[String]): Unit = {
    val options = new Options
    options.addOption(Option.builder.argName("apk").hasArg.longOpt("apk").desc("apk path").build)
    options.addOption(Option.builder.argName("spec").hasArg.longOpt("spec").desc("spec path").build)
    options.addOption(Option.builder.argName("out").hasArg.longOpt("out").desc("output JSON path").build)
    options.addOption("i", false, "Interactive mode")
    options.addOption("s", false, "Score mode")

    val parser = new DefaultParser
    val cmd = parser.parse(options, args)
    val apkPath = cmd.getOptionValue("apk")
    val specPath = cmd.getOptionValue("spec")
    val outPath = cmd.getOptionValue("out")
    val interactive = cmd.hasOption('i')
    val scored = cmd.hasOption('s')

    val yaml = new Yaml(new Constructor(classOf[Config]))
    val config = yaml.load(new FileInputStream("config.yaml")).asInstanceOf[Config]
    config.interactive = interactive
    config.apkPath = apkPath
    config.scored = scored

    Match.run(config, specPath, outPath)
  }
}