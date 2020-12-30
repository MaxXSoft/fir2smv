package smv

import scala.collection.mutable.{ListBuffer, HashMap}

// representing a SMV module
class Module(val name: String) {
  // clock of current module
  private var clock: Option[String] = None

  // all port variable declarations
  private val ports = ListBuffer[Port]()

  // all register variable declarations
  private val regs = ListBuffer[Register]()

  // all wire variable declarations
  private val wires = ListBuffer[Wire]()

  // all declared variables
  private val vars = HashMap[String, Variable]()

  // check clock in current module
  def checkClock(name: String): Unit = clock match {
    case None => clock = Some(name)
    case Some(n) => require(n == name, "multi-clock are not supported")
  }

  // add a port declaration
  def addPort(name: String, portType: Type): Unit = {
    val port = Port(portType, name)
    ports += port
    vars += (name -> port)
  }

  // add a register declaration
  def addReg(name: String, regType: Type, reset: IR, init: IR): Unit = {
    val reg = Register(regType, name, reset, init)
    regs += reg
    vars += (name -> reg)
  }

  // add a wire declaration
  def addWire(name: String, value: IR): Unit = {
    val wire = Wire(value.irType, name)
    wire.connect = Some(value)
    wires += wire
    vars += (name -> wire)
  }

  // get the specific variable
  def getVariable(name: String): Option[Variable] = vars.get(name)

  // serialize
  def serialize: String = {
    def flattenDeclaration[T <: Variable](m: ListBuffer[T]): String =
      m.map { case ir => ir.declaration }.mkString("\n    ")

    def flattenAssign[_ <: Variable](ms: ListBuffer[_]*): String = {
      ms.filter { !_.isEmpty }.flatMap {
        _.map { case ir => ir.asInstanceOf[Variable].assignment }
         .filter { _ != None }.map { _.get }
      }.mkString("\n    ")
    }

    s"""MODULE $name
       |  VAR
       |    -- Ports
       |    ${flattenDeclaration(ports)}
       |
       |    -- Registers
       |    ${flattenDeclaration(regs)}
       |
       |    -- Wires
       |    ${flattenDeclaration(wires)}
       |
       |  ASSIGN
       |    ${flattenAssign(ports, regs, wires)}
     """.stripMargin
  }
}
