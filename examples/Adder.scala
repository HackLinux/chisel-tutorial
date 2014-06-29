package TutorialExamples

import Chisel._

//A n-bit adder with carry in and carry out
class Adder(val n:Int) extends Module {
  val io = new Bundle {
    val A    = UInt(INPUT, n)
    val B    = UInt(INPUT, n)
    val Cin  = UInt(INPUT, 1)
    val Sum  = UInt(OUTPUT, n)
    val Cout = UInt(OUTPUT, 1)
  }
  //create a vector of FullAdders
  val FAs   = Vec.fill(n){ Module(new FullAdder()).io }
  val carry = Vec.fill(n+1){ UInt(width = 1) }
  val sum   = Vec.fill(n){ Bool() }

  //first carry is the top level carry in
  carry(0) := io.Cin

  //wire up the ports of the full adders
  for (i <- 0 until n) {
    FAs(i).a := io.A(i)
    FAs(i).b := io.B(i)
    FAs(i).cin := carry(i)
    carry(i+1) := FAs(i).cout
    sum(i) := FAs(i).sum.toBool()
  }
  io.Sum := sum.toBits().toUInt()
  io.Cout := carry(n)

  for (i <- 1 until n + 1)
    counter(Posedge, carry(i))
  for (i <- 1 until n)
    counter(Negedge, sum(i))
}

class AdderTests(c: Adder) extends Tester(c) {
  for (t <- 0 until 4) {
    val rnd0 = rnd.nextInt(c.n)
    val rnd1 = rnd.nextInt(c.n)
    val rnd2 = rnd.nextInt(1)

    poke(c.io.A, rnd0)
    poke(c.io.B, rnd1)
    poke(c.io.Cin, rnd2)
    step(1)
    val rsum = UInt(rnd0 + rnd1 + rnd2, width=c.n + 1)
    expect(c.io.Sum, rsum(c.n - 1, 0).litValue())
    expect(c.io.Cout, rsum(c.n).litValue())
  }
}

// same as AdderTests but extends DaisyTester
class AdderDaisyTests(c: Adder) extends DaisyTester(c) {
  for (t <- 0 until 4) {
    val rnd0 = rnd.nextInt(c.n)
    val rnd1 = rnd.nextInt(c.n)
    val rnd2 = rnd.nextInt(1)

    poke(c.io.A, rnd0)
    poke(c.io.B, rnd1)
    poke(c.io.Cin, rnd2)
    step(1)
    val rsum = UInt(rnd0 + rnd1 + rnd2, width=c.n + 1)
    expect(c.io.Sum, rsum(c.n - 1, 0).litValue())
    expect(c.io.Cout, rsum(c.n).litValue())
  }
}

class AdderWrapper(n: Int) extends DaisyWrapper(new Adder(n)) 

class AdderWrapperTests(c: AdderWrapper) extends DaisyWrapperTester(c) {
  for (t <- 0 until 4) {
    val rnd0 = rnd.nextInt(c.top.n)
    val rnd1 = rnd.nextInt(c.top.n)
    val rnd2 = rnd.nextInt(1)

    poke(c.top.io.A, rnd0)
    poke(c.top.io.B, rnd1)
    poke(c.top.io.Cin, rnd2)
    step(1)
    val rsum = UInt(rnd0 + rnd1 + rnd2, width=c.n + 1)
    expect(c.top.io.Sum, rsum(c.top.n - 1, 0).litValue())
    expect(c.top.io.Cout, rsum(c.top.n).litValue())
  }
}
