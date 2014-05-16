package TutorialExamples

import Chisel._

class FullAdder extends Module {
  val io = new Bundle {
    val a    = UInt(INPUT, 1)
    val b    = UInt(INPUT, 1)
    val cin  = UInt(INPUT, 1)
    val sum  = UInt(OUTPUT, 1)
    val cout = UInt(OUTPUT, 1)
  }

  // Generate the sum
  val a_xor_b = io.a ^ io.b
  io.sum := a_xor_b ^ io.cin
  // Generate the carry
  val a_and_b = io.a & io.b
  val b_and_cin = io.b & io.cin
  val a_and_cin = io.a & io.cin
  io.cout := a_and_b | b_and_cin | a_and_cin
  counter(Activity, a_xor_b, a_and_b, b_and_cin, a_and_cin)
  counter(Ones, io.sum)
}

class FullAdderTests(c: FullAdder) extends Tester(c, isLoggingPokes = true) {  
  for (t <- 0 until 4) {
    val a    = rnd.nextInt(2)
    val b    = rnd.nextInt(2)
    val cin  = rnd.nextInt(2)
    val res  = a + b + cin
    val sum  = res & 1
    val cout = (res >> 1) & 1
    poke(c.io.a, a)
    poke(c.io.b, b)
    poke(c.io.cin, cin)
    step(1)
    expect(c.io.sum, sum)
    expect(c.io.cout, cout)
  }
}

// same as FullAdderTests but extends DaisyTester
class FullAdderDaisyTests(c: FullAdder) extends DaisyTester(c) {  
  for (t <- 0 until 4) {
    val a    = rnd.nextInt(2)
    val b    = rnd.nextInt(2)
    val cin  = rnd.nextInt(2)
    val res  = a + b + cin
    val sum  = res & 1
    val cout = (res >> 1) & 1
    poke(c.io.a, a)
    poke(c.io.b, b)
    poke(c.io.cin, cin)
    step(1)
    expect(c.io.sum, sum)
    expect(c.io.cout, cout)
  }
}

class FullAdderWrapper extends DaisyWrapper(new FullAdder)

class FullAdderWrapperTests(c: FullAdderWrapper) extends DaisyWrapperTester(c) {
  for (t <- 0 until 4) {
    val a    = rnd.nextInt(2)
    val b    = rnd.nextInt(2)
    val cin  = rnd.nextInt(2)
    val res  = a + b + cin
    val sum  = res & 1
    val cout = (res >> 1) & 1
    poke(c.top.io.a, a)
    poke(c.top.io.b, b)
    poke(c.top.io.cin, cin)
    step(1)
    expect(c.top.io.sum, sum)
    expect(c.top.io.cout, cout)
  }
} 
