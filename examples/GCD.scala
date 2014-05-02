package TutorialExamples

import Chisel._

class GCD extends Module {
  val io = new Bundle {
    val a  = UInt(INPUT,  16)
    val b  = UInt(INPUT,  16)
    val e  = Bool(INPUT)
    val z  = UInt(OUTPUT, 16)
    val v  = Bool(OUTPUT)
  }
  val x  = Reg(UInt())
  val y  = Reg(UInt())
  when   (x > y) { x := x - y }
  unless (x > y) { y := y - x }
  when (io.e) { x := io.a; y := io.b }
  io.z := x
  io.v := y === UInt(0)
  counter(Ones, io.a, io.b)
  counter(Zeros, io.e)
  counter(Activity, io.z)
  counter(Activity, io.v)
  counter(Default, x, y)
}

class GCDTests(c: GCD) extends Tester(c) {
  val (a, b, z) = (64, 48, 16)
  do {
    val first = if (t == 0) 1 else 0;
    poke(c.io.a, a)
    poke(c.io.b, b)
    poke(c.io.e, first)
    step(1)
  } while (t <= 1 || peek(c.io.v) == 0)
  expect(c.io.z, z)
}

// same as GCDTests but extends DaisyTester
class GCDDaisyTests(c: GCD) extends DaisyTester(c) {
  val (a, b, z) = (64, 48, 16)
  do {
    val first = if (t == 0) 1 else 0;
    poke(c.io.a, a)
    poke(c.io.b, b)
    poke(c.io.e, first)
    step(1)
  } while (t <= 1 || peek(c.io.v) == 0)
  expect(c.io.z, z)
}

class GCDWrapper extends DaisyWrapper(new GCD) {
  // write 0 -> { GCD.io.a, GCD.io.b }
  top.io.a := wdata(0)(31, 16)
  top.io.b := wdata(0)(15, 0)

  // write 1 -> GCD.io.e
  top.io.e := wdata(1)(0)

  // read 0 -> GCD.io.z
  rdata(0)  := top.io.z

  // read 1 -> GCD.io.v
  rdata(1)  := top.io.v
}

class GCDWrapperTests(c: GCDWrapper) extends DaisyWrapperTester(c) {
  val (a, b, z) = (64, 48, 16)
  pokeAddr(0, a << 16 | b)
  do {
    val first = if (t == 0) 1 else 0;
    pokeAddr(1, first)
    step(1)
  } while (t <= 1 || peekAddr(1) == 0)
  expectAddr(0, z)
}
