package gcd

import chisel3._
import chisel3.util.Decoupled

class GcdInputBundle(val w: Int) extends Bundle {
  val value1 = UInt(w.W)
  val value2 = UInt(w.W)
}

class GcdOutputBundle(val w: Int) extends Bundle {
  val value1 = UInt(w.W)
  val value2 = UInt(w.W)
  val gcd    = UInt(w.W)
}

/**
  * Compute Gcd using subtraction method.
  * Subtracts the smaller from the larger until register y is zero.
  * value input register x is then the Gcd.
  * Unless first input is zero then the Gcd is y.
  * Can handle stalls on the producer or consumer side
  */
class DecoupledGCD(width: Int) extends MultiIOModule {
  val input = IO(Flipped(Decoupled(new GcdInputBundle(width))))
  val output = IO(Decoupled(new GcdOutputBundle(width)))

  val xInitial    = Reg(UInt())
  val yInitial    = Reg(UInt())
  val x           = Reg(UInt())
  val y           = Reg(UInt())
  val busy        = RegInit(false.B)
  val resultValid = RegInit(false.B)

  input.ready := ! busy
  output.valid := resultValid
  output.bits := DontCare

  when(busy)  {
    when(x > y) {
      x := x - y
    }.otherwise {
      y := y - x
    }
    when(x === 0.U || y === 0.U) {
      when(x === 0.U) {
        output.bits.gcd := y
      }.otherwise {
        output.bits.gcd := x
      }

      output.bits.value1 := xInitial
      output.bits.value2 := yInitial
      resultValid := true.B

      when(output.ready && resultValid) {
        busy := false.B
        resultValid := false.B
      }
    }
  }.otherwise {
    when(input.valid) {
      val bundle = input.deq()
      x := bundle.value1
      y := bundle.value2
      xInitial := bundle.value1
      yInitial := bundle.value2
      busy := true.B
    }
  }
}

object DecoupledGCD extends App {
  import emitter.{FirrtlEmitter, SmvEmitter}
  import smv.ltl._

  val circuit = FirrtlEmitter(() => new DecoupledGCD(5))
  val smvFile = SmvEmitter(circuit)

  val reset = AnyRef[DecoupledGCD]("reset")
  val iready = Ref[DecoupledGCD](_.input.ready)
  val ivalid = Ref[DecoupledGCD](_.input.valid)
  val oready = Ref[DecoupledGCD](_.output.ready)
  val ovalid = Ref[DecoupledGCD](_.output.valid)
  val condReset = reset === true.LB & X(G(reset === false.LB))
  val condReady = iready === true.LB
  val condValid = ivalid === true.LB & X(G(ivalid === false.LB))
  val condOutput = G(oready === true.LB)
  val cond = condReset & X(condReady & condValid & condOutput)
  smvFile.addLtlSpec(cond -> F(ovalid === true.LB))

  println(smvFile.serialize)
}
