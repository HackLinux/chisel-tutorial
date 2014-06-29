package TutorialExamples

import Chisel._

class Parity extends Module {
  val io = new Bundle {
    val in  = Bool(INPUT)
    val out = Bool(OUTPUT) }
  val s_even :: s_odd :: Nil = Enum(UInt(), 2)
  val state  = Reg(init=s_even)
  when (io.in) {
    when (state === s_even) { state := s_odd  }
    .otherwise              { state := s_even }
  }
  io.out := (state === s_odd)
  counter(Ones, io.in)
  counter(Zeros, io.out)
  counter(Activity, state)
}

class ParityTests(c: Parity) extends Tester(c, false, isLoggingPokes = true) {
  var isOdd = 0
  for (t <- 0 until 200) {
    val bit = rnd.nextInt(2)
    poke(c.io.in, bit)
    step(1)
    expect(c.io.out, isOdd)
    isOdd = (isOdd + bit) % 2;
  }
}

// same as ParityTests but extends DaisyTester
class ParityDaisyTests(c: Parity) extends DaisyTester(c) {
  var isOdd = 0
  for (t <- 0 until 200) {
    val bit = rnd.nextInt(2)
    poke(c.io.in, bit)
    step(1)
    expect(c.io.out, isOdd)
    isOdd = (isOdd + bit) % 2;
  }
}

class ParityWrapper extends DaisyWrapper(new Parity)

class ParityWrapperTests(c: ParityWrapper) extends DaisyWrapperTester(c, false) {
  var isOdd = 0
  for (t <- 0 until 200) {
    val bit = rnd.nextInt(2)
    poke(c.top.io.in, bit)
    step(1)
    expect(c.top.io.out, isOdd)
    isOdd = (isOdd + bit) % 2;
  }
}
