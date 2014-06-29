package TutorialExamples

import Chisel._

class ByteSelector extends Module {
  val io = new Bundle {
    val in     = UInt(INPUT, 32)
    val offset = UInt(INPUT, 2)
    val out    = UInt(OUTPUT, 8)
  }
  val a = io.in(7,0)
  val b = io.in(15,8)
  val c = io.in(23,16)
  val d = io.in(31,24)

  io.out := UInt(0, width=8)
  when (io.offset === UInt(0, width=2)) {
    io.out := io.in(7,0)
  } .elsewhen (io.offset === UInt(1)) {
    io.out := io.in(15,8)
  } .elsewhen (io.offset === UInt(2)) {
    io.out := io.in(23,16)
  } .otherwise {
    io.out := io.in(31,24)
  }

  counter(Negedge, io.offset)
}

class ByteSelectorTests(c: ByteSelector) extends Tester(c) {
  val test_in = 12345678
  for (t <- 0 until 4) {
    poke(c.io.in,     test_in)
    poke(c.io.offset, t)
    step(1)
    expect(c.io.out, (test_in >> (t * 8)) & 0xFF)
  }
}

// same as ByteSelectorTests but extends DaisyTester
class ByteSelectorDaisyTests(c: ByteSelector) extends DaisyTester(c) {
  val test_in = 12345678
  for (t <- 0 until 4) {
    poke(c.io.in,     test_in)
    poke(c.io.offset, t)
    step(1)
    expect(c.io.out, (test_in >> (t * 8)) & 0xFF)
  }
}

class ByteSelectorWrapper extends DaisyWrapper(new ByteSelector)

class ByteSelectorWrapperTests(c: ByteSelectorWrapper) extends DaisyWrapperTester(c) {
  val test_in = 12345678
  for (t <- 0 until 4) {
    poke(c.top.io.in,     test_in)
    poke(c.top.io.offset, t)
    step(1)
    expect(c.top.io.out, (test_in >> (t * 8)) & 0xFF)
  }
}
