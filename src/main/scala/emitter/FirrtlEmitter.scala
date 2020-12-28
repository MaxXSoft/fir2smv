package emitter

import chisel3._
import chisel3.stage.ChiselStage

import firrtl.{CircuitState, Parser, LowFirrtlEmitter}
import firrtl.ir.Circuit
import firrtl.options.Dependency
import firrtl.stage.transforms.Compiler

object FirrtlEmitter {
  def apply[T <: Module](gen: () => T): Circuit = {
    // get chirrtl input
    val input = (new ChiselStage).emitChirrtl(gen())
    // parse the input
    val state = CircuitState(Parser.parse(input), Seq())
    // compile to low-firrtl
    val compiler = new Compiler(Seq(Dependency[LowFirrtlEmitter]))
    val finalState = compiler.execute(state)
    // emit output
    finalState.circuit
  }
}
