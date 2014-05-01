package TutorialExamples

import Chisel._
import scala.collection.mutable.HashMap
import scala.math._

class ClockedAccumulator(c: Clock) extends Module(clock = c) {
  val io = new Bundle {
    val inc = Decoupled(UInt(width = 32)).flip()
    val sum = Decoupled(UInt(width = 32))
  }
  val accumulator = Reg(init = UInt(0, 32))
  when (io.inc.valid && io.sum.ready) {
    accumulator := accumulator + io.inc.bits
  }
  io.inc.ready := io.sum.ready
  io.sum.valid := io.inc.valid
  io.sum.bits  := accumulator
  counter(Activity, accumulator)
}

class MultiClockDomain extends Module {
  val io = new Bundle {
    val start = Bool(INPUT)
    val sum   = Decoupled(UInt(OUTPUT))
  }
  val fastClock = new Clock()
  val slowClock = new Clock()

  val a0 = Module(new ClockedAccumulator(fastClock))
  a0.io.inc.valid := io.start
  a0.io.inc.bits  := UInt(1)

  val asyncFifo = Module(new AsyncFifo(UInt(width=32), 32, fastClock, slowClock))
  asyncFifo.io.enq <> a0.io.sum

  val a1 = Module(new ClockedAccumulator(slowClock))
  a1.io.inc <> asyncFifo.io.deq
  io.sum.bits     := a1.io.sum.bits
  io.sum.valid    := a1.io.sum.valid
  a1.io.sum.ready := io.sum.ready
  counter(Activity, io.start)
}

class MultiClockDomainTests(c: MultiClockDomain) extends Tester(c) {
  // setting up clocks
  val clocks = new HashMap[Clock, Int]
  clocks(Driver.implicitClock) = 2
  clocks(c.fastClock) = 4
  clocks(c.slowClock) = 6
  setClocks(clocks)

  // out of reset, but not starting accumulators yet
  for (i <- 0 until 5) {
    poke(c.io.start,     0)
    poke(c.io.sum.ready, 0)
    step(1)
  }

  val answers = Array(0, 0, 1, 3, 6, 10, 15, 21, 28, 36)
  while (t < 10) {
    poke(c.io.start,     1)
    poke(c.io.sum.ready, 1)
    step(1)
    println("DELTA " + delta)
    // only check outputs on valid && 6 deltas have passed
    if (peek(c.io.sum.valid) == 1 && (delta % 6 == 0)) {
      expect(c.io.sum.bits, answers(t))
    }
  }
}

// same as MultiClockDomainTests but extends DaisyTester
class MultiClockDomainDaisyTests(c: MultiClockDomain) extends DaisyTester(c) {
  // setting up clocks
  val clocks = new HashMap[Clock, Int]
  clocks(Driver.implicitClock) = 2
  clocks(c.fastClock) = 4
  clocks(c.slowClock) = 6
  setClocks(clocks)

  // out of reset, but not starting accumulators yet
  for (i <- 0 until 5) {
    poke(c.io.start,     0)
    poke(c.io.sum.ready, 0)
    step(1)
  }

  val answers = Array(0, 0, 1, 3, 6, 10, 15, 21, 28, 36)
  while (t < 10) {
    poke(c.io.start,     1)
    poke(c.io.sum.ready, 1)
    step(1)
    println("DELTA " + delta)
    // only check outputs on valid && 6 deltas have passed
    if (peek(c.io.sum.valid) == 1 && (delta % 6 == 0)) {
      expect(c.io.sum.bits, answers(t))
    }
  }
}

class MultiClockDomainWrapper extends DaisyWrapper(new MultiClockDomain) {
  // write(0) -> { MultiClockDomain.io.sum.ready, MultiClockDomain.io.start }
  val in_reg = Reg(UInt())
  when (wen(0)) {
    in_reg := io.in.bits
  }
  top.io.start     := in_reg(0)
  top.io.sum.ready := in_reg(1)

  // read(0) -> MultiClockDomain.io.sum.bits
  rdata(0) := top.io.sum.bits
  rvalid(0) := Bool(true)

  // rdata(1) -> MultiClockDomain.io.sum.valid
  rdata(1) := top.io.sum.valid
  rvalid(1) := Bool(true)
}

class MultiClockDomainWrapperTests(c: MultiClockDomainWrapper) extends DaisyWrapperTester(c) {
  // setting up clocks
  val clocks = new HashMap[Clock, Int]
  clocks(Driver.implicitClock) = 2
  clocks(c.top.fastClock) = 4
  clocks(c.top.slowClock) = 6
  setClocks(clocks)

  // out of reset, but not starting accumulators yet
  for (i <- 0 until 5) {
    // c.io.start <- 0, c.io.sum.ready <-0
    pokeAddr(0, 0 | 0 << 1)
    step(1)
  }

  val answers = Array(0, 0, 1, 3, 6, 10, 15, 21, 28, 36)
  while (t < 10) {
    // c.io.start <- 1, c.io.sum.ready <-1
    pokeAddr(0, 1 | 1 << 1)
    step(1)
    println("DELTA " + delta)
    // only check outputs on valid && 6 deltas have passed
    if (peekAddr(1) == 1 && (delta % 6 == 0)) {
      expectAddr(0, answers(t)) // c.io.sum.bits = ?
    }
  }
}
