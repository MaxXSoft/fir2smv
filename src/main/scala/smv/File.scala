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
  def addLtlSpec(spec: Value): Unit = {
    ltlSpecs += spec
  }

  // serialize
  def serialize: String = {
    s"""${modules.map { _.serialize }.mkString("\n")}
       |
       |MODULE main
       |  // TODO
       |
       |  ${ltlSpecs.map { v => s"LTLSPEC ${v.reference}" }.mkString("\n")}
     """.stripMargin
  }
}
