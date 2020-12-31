package smv

import scala.collection.mutable.ListBuffer

class File {
  // all declared modules
  private val modules = ListBuffer[Module]()

  // LTL specifications
  private val ltlSpecs = ListBuffer[spec.Value]()

  // CTL specifications
  private val ctlSpecs = ListBuffer[spec.Value]()

  // add a new module
  def addModule(module: Module): Unit = {
    modules += module
  }

  // add a new LTL specification
  def addLtlSpec[T](value: spec.Value): Unit = {
    ltlSpecs += value
  }

  // add a new CTL specification
  def addCtlSpec[T](value: spec.Value): Unit = {
    ctlSpecs += value
  }

  // serialize
  def serialize: String = {
    val moduleDefs = modules.map { _.serialize }.mkString("\n")
    val moduleInsts = modules.map {
      m => s"${File.getInstanceName(m.name)}: ${m.name};"
    }.mkString("\n")
    val specStr = (ltlSpecs.map {
      v => s"LTLSPEC ${v.serialize}"
    } ++ ctlSpecs.map {
      v => s"CTLSPEC ${v.serialize}"
    }).mkString("\n")

    s"""$moduleDefs
       |
       |MODULE main
       |  VAR
       |    $moduleInsts
       |
       |  $specStr
     """.stripMargin
  }
}

object File {
  // get instance name by module name
  def getInstanceName(name: String): String = {
    val s = name.replaceAll("([A-Z]+)([A-Z][a-z])", "$1_$2")
                .replaceAll("([a-z\\d])([A-Z])", "$1_$2").toLowerCase
    s"$s"
  }
}
