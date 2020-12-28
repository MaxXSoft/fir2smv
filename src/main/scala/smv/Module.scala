package smv

import scala.collection.mutable.HashMap

class Module(val name: String) {
  // clock of current module
  private var clock: Option[String] = None

  // all port variable declarations
  private val ports = HashMap[String, Type]()

  // all register variable declarations
  private val regs = HashMap[String, Type]()

  // all wire variable declarations
  private val wires = HashMap[String, Type]()

  // all variable assignments
  private val assigns = HashMap[String, String]()

  // check clock in current module
  def checkClock(name: String): Unit = clock match {
    case None => clock = Some(name)
    case Some(n) => require(n == name, "multi-clock are not supported")
  }

  // add a port declaration
  def addPort(name: String, portType: Type): Unit = {
    ports += (name -> portType)
  }

  // add a register declaration
  def addReg(name: String, regType: Type): Unit = {
    regs += (name -> regType)
  }

  // add a wire declaration
  def addWire(name: String, wireType: Type): Unit = {
    wires += (name -> wireType)
  }

  // add a assignment
  def addAssign(name: String, expr: String): Unit = {
    assigns += (name -> expr)
  }

  // serialize
  def serialize: String = {
    def flatten[T](m: HashMap[String, T]): String = {
      m.map {
        case (s, t: Type) => s"${s}: ${t.serialize}"
        case (s, t) => s"${s}: ${t}"
      }.mkString(";\n    ")
    }

    s"""MODULE ${name}
       |  VAR
       |    -- Ports
       |    ${flatten(ports)}
       |
       |    -- Registers
       |    ${flatten(regs)}
       |
       |    -- Wires
       |    ${flatten(wires)}
       |
       |  ASSIGN
       |    ${flatten(assigns)}
     """.stripMargin
  }
}
