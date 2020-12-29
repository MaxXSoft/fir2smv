package gcd

import chisel3._

import emitter.{FirrtlEmitter, SmvEmitter}

/**
  * Compute GCD using subtraction method.
  * Subtracts the smaller from the larger until register y is zero.
  * value in register x is then the GCD
  */
class GCD(val width: Int) extends Module {
  val io = IO(new Bundle {
    val value1        = Input(UInt(width.W))
    val value2        = Input(UInt(width.W))
    val loadingValues = Input(Bool())
    val outputGCD     = Output(UInt(width.W))
    val outputValid   = Output(Bool())
  })

  val x  = Reg(UInt())
  val y  = Reg(UInt())

  when(x > y) { x := x - y }
    .otherwise { y := y - x }

  when(io.loadingValues) {
    x := io.value1
    y := io.value2
  }

  io.outputGCD := x
  io.outputValid := y === 0.U
}

object GCD extends App {
  val circuit = FirrtlEmitter(() => new GCD(5))
  val smvFile = SmvEmitter(circuit)
  println(smvFile.serialize)
}
