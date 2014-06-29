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

class GCDTests(c: GCD) extends Tester(c, isLoggingPokes = true) {
  val (a, b, z) = (2128, 2, 2)
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
class GCDDaisyTests(c: GCD) extends DaisyTester(c, false) {
  val (a, b, z) = (2128, 2, 2)
  do {
    val first = if (t == 0) 1 else 0;
    poke(c.io.a, a)
    poke(c.io.b, b)
    poke(c.io.e, first)
    step(1)
  } while (t <= 1 || peek(c.io.v) == 0)
  expect(c.io.z, z)
}

class GCDWrapper extends DaisyWrapper(new GCD)

class GCDWrapperTests(c: GCDWrapper) extends DaisyWrapperTester(c, false) {
  val (a, b, z) = (2128, 2, 2)
  do {
    val first = if (t == 0) 1 else 0;
    poke(c.top.io.a, a)
    poke(c.top.io.b, b)
    poke(c.top.io.e, first)
    step(1)
  } while (t <= 1 || peek(c.top.io.v) == 0)
  expect(c.top.io.z, z)
}
