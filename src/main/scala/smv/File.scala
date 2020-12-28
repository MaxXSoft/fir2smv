package smv

import scala.collection.mutable.ListBuffer

class File {
  // all declared modules
  private val modules = ListBuffer[Module]()

  // LTL specifications
  private val ltlSpecs = ListBuffer[String]()

  // add a new module
  def addModule(module: Module): Unit = {
    modules += module
  }

  // add a new LTL specification
  def addLtlSpec(spec: String): Unit = {
    ltlSpecs += spec
  }

  // serialize
  def serialize: String = {
    s"""${modules.map { m => m.serialize }.mkString("\n")}
       |
       |MODULE main
       |  // TODO
     """.stripMargin
  }
}
