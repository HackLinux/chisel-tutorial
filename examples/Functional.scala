package TutorialExamples

import Chisel._

class Functional extends Module {
  val io = new Bundle {
    val x   = Bits(INPUT,  16)
    val y   = Bits(INPUT,  16)
    val z   = Bits(OUTPUT, 16)
  }
  def clb(a: Bits, b: Bits, c: Bits, d: Bits) =
    (a & b) | (~c & d)
  io.z := clb(io.x, io.y, io.x, io.y)
  counter(Activity, io.x, io.y)
  counter(io.z)
}

class FunctionalTests(c: Functional) extends Tester(c) {
  val maxInt = 1 << 16
  for (i <- 0 until 10) {
    val x = rnd.nextInt(maxInt)
    val y = rnd.nextInt(maxInt)
    poke(c.io.x, x)
    poke(c.io.y, y)
    step(1)
    expect(c.io.z, (x & y) | (~x & y))
  }
}

// same as FunctionalTests but extends DaisyTester
class FunctionalDaisyTests(c: Functional) extends DaisyTester(c) {
  val maxInt = 1 << 16
  for (i <- 0 until 10) {
    val x = rnd.nextInt(maxInt)
    val y = rnd.nextInt(maxInt)
    poke(c.io.x, x)
    poke(c.io.y, y)
    step(1)
    expect(c.io.z, (x & y) | (~x & y))
  }
}

class FunctionalWrapper extends DaisyWrapper(new Functional)

class FunctionalWrapperTests(c: FunctionalWrapper) extends DaisyWrapperTester(c) {
  val maxInt = 1 << 16
  for (i <- 0 until 10) {
    val x = rnd.nextInt(maxInt)
    val y = rnd.nextInt(maxInt)
    poke(c.top.io.x, x)
    poke(c.top.io.y, y)
    step(1)
    expect(c.top.io.z, (x & y) | (~x & y))
  }
}
