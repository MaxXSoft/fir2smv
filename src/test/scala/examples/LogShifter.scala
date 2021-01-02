// Added by MaxXing:
// from https://github.com/ucb-bar/chisel-tutorial/blob/e567a5f27803425907af8ad528cccf3bb29e5d7d/src/main/scala/examples/LogShifter.scala

// See LICENSE.txt for license details.
package examples

import chisel3._

class LogShifter extends Module {
  val io = IO(new Bundle {
    val in    = Input(UInt(16.W))
    val shamt = Input(UInt(4.W))
    val out   = Output(UInt(16.W))
  })
  val s0 = RegInit(0.U(16.W))
  when (io.shamt(3) === 1.U) {
    s0 := io.in << 8.U
  } .otherwise {
    s0 := io.in
  }
  val s1 = RegInit(0.U(16.W))
  when (io.shamt(2) === 1.U) {
    s1 := s0 << 4.U
  } .otherwise {
    s1 := s0
  }
  val s2 = RegInit(0.U(16.W))
  when (io.shamt(1) === 1.U) {
    s2 := s1 << 2.U
  } .otherwise {
    s2 := s1
  }
  when (io.shamt(1) === 1.U) {
    io.out := s2 << 1.U
  } .otherwise {
    io.out := s2
  }
}

object LogShifter extends App {
  import emitter.{FirrtlEmitter, SmvEmitter}
  import smv.spec._

  val circuit = FirrtlEmitter(() => new LogShifter)
  val smvFile = SmvEmitter(circuit)

  val reset = AnyRef[LogShifter]("reset")
  val in = Ref[LogShifter](_.io.in)
  val shamt = Ref[LogShifter](_.io.shamt)
  val out = Ref[LogShifter](_.io.out)

  val condReset = reset === true.LB & X(G(reset === false.LB))
  val condIn = G(in === 1234.LU(16) & shamt === 1.LU(4))
  val cond = condReset & X(condIn)
  val result = F(out === (1234 << 1).LU(16))
  smvFile.addLtlSpec(cond -> result)

  println(smvFile.serialize)
}
