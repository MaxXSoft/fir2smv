// See LICENSE.txt for license details.
package examples

import chisel3._
import chisel3.util.log2Ceil

class Stack(val depth: Int) extends Module {
  val io = IO(new Bundle {
    val push    = Input(Bool())
    val pop     = Input(Bool())
    val en      = Input(Bool())
    val dataIn  = Input(UInt(32.W))
    val dataOut = Output(UInt(32.W))
  })

  val stack_mem = Mem(depth, UInt(32.W))
  val sp        = RegInit(0.U(log2Ceil(depth+1).W))
  val out       = RegInit(0.U(32.W))

  when (io.en) {
    when(io.push && (sp < depth.asUInt)) {
      stack_mem(sp) := io.dataIn
      sp := sp + 1.U
    } .elsewhen(io.pop && (sp > 0.U)) {
      sp := sp - 1.U
    }
    when (sp > 0.U) {
      out := stack_mem(sp - 1.U)
    }
  }

  io.dataOut := out
}

object Stack extends App {
  import emitter.{FirrtlEmitter, SmvEmitter}
  import smv.spec._

  val circuit = FirrtlEmitter(() => new Stack(5))
  val smvFile = SmvEmitter(circuit)

  val reset = AnyRef[Stack]("reset")
  val en = Ref[Stack](_.io.en)
  val push = Ref[Stack](_.io.push)
  val pop = Ref[Stack](_.io.pop)
  val dataIn = Ref[Stack](_.io.dataIn)
  val dataOut = Ref[Stack](_.io.dataOut)

  // Verify: push 1234 and then pop, always get 1234
  //
  // perform reset
  val condReset = reset === true.LB & X(G(reset === false.LB))
  // enable only in the first two cycles
  val condEn = en === true.LB & X(en === true.LB & X(G(en === false.LB)))
  // push 1234 in current cycle, and release push signal in next cycle
  val condPush = push === true.LB & dataIn === 1234.LU(32) &
                 X(G(push === false.LB))
  // pop a value in current cycle, and release pop signal in next cycle
  val condPop = pop === true.LB & X(G(pop === false.LB))
  // the condition: reset first, then push and pop
  val cond = condReset & X(condPush & condEn & X(condPop))
  // the result: we will get 1234 in the third cycle
  val result = X(X(X(G(dataOut === 1234.LU(32)))))
  smvFile.addLtlSpec(cond -> result)

  println(smvFile.serialize)
}
