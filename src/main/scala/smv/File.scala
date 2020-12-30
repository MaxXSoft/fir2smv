package smv

import scala.collection.mutable.ListBuffer

class File {
  // all declared modules
  private val modules = ListBuffer[Module]()

  // LTL specifications
  private val ltlSpecs = ListBuffer[Value]()

  // add a new module
  def addModule(module: Module): Unit = {
    modules += module
  }

  // add a new LTL specification
  def addLtlSpec[T](spec: Value): Unit = {
    ltlSpecs += spec
  }

  // serialize
  def serialize: String = {
    val moduleDefs = modules.map { _.serialize }.mkString("\n")
    val moduleInsts = modules.map {
      m => s"${File.getInstanceName(m.name)}: ${m.name};"
    }.mkString("\n")
    val ltlSpecStr = ltlSpecs.map {
      v => s"LTLSPEC ${v.reference}"
    }.mkString("\n")

    s"""$moduleDefs
       |
       |MODULE main
       |  VAR
       |    $moduleInsts
       |
       |  $ltlSpecStr
     """.stripMargin
  }
}

object File {
  // get instance name by module name
  def getInstanceName(name: String): String = {
    val s = name.replaceAll("([A-Z]+)([A-Z][a-z])", "$1_$2")
                .replaceAll("([a-z\\d])([A-Z])", "$1_$2").toLowerCase
    s"$$$s"
  }
}
