package TutorialExamples

import Chisel._

class ShiftRegister extends Module {
  val io = new Bundle {
    val in  = UInt(INPUT,  1)
    val out = UInt(OUTPUT, 1)
  }
  val r0 = Reg(next = io.in)
  val r1 = Reg(next = r0)
  val r2 = Reg(next = r1)
  val r3 = Reg(next = r2)
  io.out := r3
  counter(Ones, r0, r1, r2, r3)
}

class ShiftRegisterTests(c: ShiftRegister) extends Tester(c, isLoggingPokes = true) {  
  val reg     = Array.fill(4){ 0 }
  for (t <- 0 until 200) {
    val in = rnd.nextInt(2)
    poke(c.io.in, in)
    step(1)
    if (t >= 4) expect(c.io.out, reg(3))
    for (i <- 3 to 1 by -1)
      reg(i) = reg(i-1)
    reg(0) = in
  }
}

// same as ShiftRegisterTests but extends DaisyTester
class ShiftRegisterDaisyTests(c: ShiftRegister) extends DaisyTester(c, false) {  
  val reg     = Array.fill(4){ 0 }
  for (t <- 0 until 200) {
    val in = rnd.nextInt(2)
    poke(c.io.in, in)
    step(1)
    if (t >= 4) expect(c.io.out, reg(3))
    for (i <- 3 to 1 by -1)
      reg(i) = reg(i-1)
    reg(0) = in
  }
}

class ShiftRegisterWrapper extends DaisyWrapper(new ShiftRegister)

class ShiftRegisterWrapperTests(c: ShiftRegisterWrapper) extends DaisyWrapperTester(c, false) {
  val reg     = Array.fill(4){ 0 }
  for (t <- 0 until 200) {
    val in = rnd.nextInt(2)
    poke(c.top.io.in, in)
    step(1)
    if (t >= 4) expect(c.top.io.out, reg(3))
    for (i <- 3 to 1 by -1)
      reg(i) = reg(i-1)
    reg(0) = in
  }
}
