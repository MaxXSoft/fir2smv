package smv

import scala.collection.mutable.HashMap

// representing a SMV module
class Module(val name: String) {
  // clock of current module
  private var clock: Option[String] = None

  // all port variable declarations
  private val ports = HashMap[String, Port]()

  // all register variable declarations
  private val regs = HashMap[String, Register]()

  // all wire variable declarations
  private val wires = HashMap[String, Wire]()

  // check clock in current module
  def checkClock(name: String): Unit = clock match {
    case None => clock = Some(name)
    case Some(n) => require(n == name, "multi-clock are not supported")
  }

  // add a port declaration
  def addPort(name: String, portType: Type): Unit = {
    ports += (name -> Port(portType, name))
  }

  // get the specific port declaration
  def getPort(name: String): Option[Port] = ports.get(name)

  // add a register declaration
  def addReg(name: String, regType: Type, reset: IR, init: IR): Unit = {
    regs += (name -> Register(regType, name, reset, init))
  }

  // get the specific register declaration
  def getReg(name: String): Option[Register] = regs.get(name)

  // add a wire declaration
  def addWire(name: String, wireType: Type): Unit = {
    wires += (name -> Wire(wireType, name))
  }

  // get the specific wire declaration
  def getWire(name: String): Option[Wire] = wires.get(name)

  // serialize
  def serialize: String = {
    def flattenDeclaration[T <: Variable](m: HashMap[String, T]): String =
      m.map { case (_, ir) => ir.declaration }.mkString("\n    ")

    def flattenAssign[_ <: Variable](ms: HashMap[String, _]*): String = {
      ms.filter { !_.isEmpty }.flatMap {
        _.map { case (_, ir) => ir.asInstanceOf[Variable].assignment }
         .filter { _ != None }
      }.mkString("\n    ")
    }

    s"""MODULE ${name}
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
