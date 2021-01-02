package smv

import scala.collection.mutable.{ListBuffer, HashMap}

// representing a SMV module
class Module(val file: File, val name: String) {
  // clock of current module
  private var clock: Option[String] = None

  // all port variable declarations
  private val ports = ListBuffer[Port]()

  // all register variable declarations
  private val regs = ListBuffer[Register]()

  // all wire variable declarations
  private val wires = ListBuffer[Wire]()

  // all memory port variable declarations
  private val memPorts = ListBuffer[MemoryPort]()

  // all declared variables
  private val vars = HashMap[String, Variable]()

  // all memory declarations (name, type, awidth)
  private val mems = ListBuffer[(String, Memory, Int)]()

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

  // add a memory port declaration
  def addMemPort(name: String, value: IR): Unit = {
    val memPort = MemoryPort(value.irType, name)
    memPort.connect = Some(value)
    memPorts += memPort
    vars += (name -> memPort)
  }

  // add a memory declaration
  def addMemory(name: String, dataType: Type, depth: BigInt, wlat: Int,
                rlat: Int, reader: String, writer: String): Unit = {
    val addrWidth = chisel3.util.log2Ceil(depth)
    val mem = MemoryModule(dataType, wlat, rlat, reader, writer)
    file.addMemory(Reader(dataType, rlat))
    file.addMemory(Writer(dataType, wlat))
    file.addMemory(mem)
    val a = (name, mem, addrWidth)
    mems += a
  }

  // get the specific variable
  def getVariable(name: String): Option[Variable] = vars.get(name)

  // serialize
  def serialize: String = {
    def flattenVarDecl[T <: Variable](l: ListBuffer[T]): String =
      if (l.nonEmpty) l.map(_.declaration).mkString("\n    ") else "-- NONE"
    
    def flattenMemDecl(l: ListBuffer[(String, Memory, Int)]) = {
      if (l.nonEmpty) {
        l.map {
          case (name, mem, awidth) => s"$name: ${mem.typeName}($awidth);"
        }.mkString("\n    ")
      } else {
        "-- NONE"
      }
    }

    def flattenAssign[_ <: Variable](ls: ListBuffer[_]*): String = {
      ls.filter { !_.isEmpty }.flatMap {
        _.map { _.asInstanceOf[Variable].assignment }
         .filter { _ != None }.map { _.get }
      }.mkString("\n    ")
    }

    s"""MODULE $name
       |  VAR
       |    -- Ports
       |    ${flattenVarDecl(ports)}
       |
       |    -- Registers
       |    ${flattenVarDecl(regs)}
       |
       |    -- Wires
       |    ${flattenVarDecl(wires)}
       |
       |    -- Memories
       |    ${flattenMemDecl(mems)}
       |
       |  ASSIGN
       |    ${flattenAssign(ports, regs, wires, memPorts)}
     """.stripMargin
  }
}
