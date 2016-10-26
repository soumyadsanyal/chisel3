// See LICENSE for license details.

package cookbook

import chisel3._
import chisel3.util._

/* ### How do I create a finite state machine?
 *
 * Use Chisel Enum to construct the states and switch & is to construct the FSM
 * control logic
 */
// Common interface for simple vending machines
abstract class VendingMachine extends Module {
  val io = IO(new Bundle {
    val nickel = Input(Bool())
    val dime   = Input(Bool())
    val valid  = Output(Bool())
  })
  assert(!(io.nickel && io.dime), "Only one of nickel or dime can be input at a time!")
}

// Vending machine implemented with a Finite State Machine
class VendingMachineFSM extends VendingMachine {
  val sIdle :: s5 :: s10 :: s15 :: sOk :: Nil = Enum(5)
  val state = Reg(init = sIdle)

  switch (state) {
    is (sIdle) {
      when (io.nickel) { state := s5 }
      when (io.dime)   { state := s10 }
    }
    is (s5) {
      when (io.nickel) { state := s10 }
      when (io.dime)   { state := s15 }
    }
    is (s10) {
      when (io.nickel) { state := s15 }
      when (io.dime)   { state := sOk }
    }
    is (s15) {
      when (io.nickel) { state := sOk }
      when (io.dime)   { state := sOk }
    }
    is (sOk) {
      state := sIdle
    }
  }
  io.valid := (state === sOk)
}

/* While explicit finite state machines are an important pattern in digital
 * design, we find that parameterized generators often employ *implicit* state
 * machines. The following example illustrates how the same vending machine as
 * the above might be implemented in a more parameterizable way. Changing the
 * soda cost and the coins accepted is as simple as changing the `coins` and
 * `sodaCost` parameters below. It is left as an exercise for the reader to
 * extend both of these implementations to accept Quarters and charge $1.50 per
 * soda.
 */
// Vending machine implemented without a finite state machine
class VendingMachineNoFSM extends VendingMachine {
  /* Parameters
   *  - Coin values must all be multiples of minimum value)
   *  - Since coins are divisible by min, we can normalize all values to multiples min value
   */
  val coins = Seq((io.nickel, 5), (io.dime, 10)) sortBy (_._2)
  val minCoin = coins.head._2
  val maxCoin = coins.last._2
  val sodaCost = 20
  val maxValue = (sodaCost + maxCoin - minCoin) / minCoin // normalized

  val value = Reg(init = 0.asUInt(log2Up(maxValue).W))
  val incValue = Wire(init = 0.asUInt(log2Up(maxValue).W))
  val dispense = value >= (sodaCost / minCoin).U

  when (dispense) {
    value := 0.U // No change
  } .otherwise {
    value := value + incValue
  }

  for ((cond, worth) <- coins) {
    when (cond) { incValue := (worth / minCoin).U }
  }
  io.valid := dispense
}

/* Useful utilities for testing vending machines */
object VendingMachineUtils {
  abstract class Coin(val value: Int)
  case object NoCoin extends Coin(0)
  case object Nickel extends Coin(5)
  case object Dime extends Coin(10)

  // Calculate expected outputs by accumulating inputs, reseting to 0 when $.20 is reached
  def getExpectedResults(inputs: Seq[Coin]): Seq[Boolean] = {
    inputs.scanLeft((0, false)) { // (coin sum, soda dispensed)
      case ((in, _), x) => if (in + x.value >= 20) (0, true) else (in + x.value, false)
    } map (_._2) // drop the current value, just get expected output
  }

}

/** This tester is parameterized with a given implementation of [[VendingMachine]]
  * We pass mod by reference so that we only invoke the Chisel construction of the Module once
  */
class VendingMachineTester(mod: => VendingMachine) extends CookbookTester(10) {
  import VendingMachineUtils._

  // Construct module
  val dut = Module(mod)

  // Inputs and expected results
  val inputs: Seq[Coin] = Seq(Nickel, Dime, Dime, NoCoin, Nickel, Nickel, Nickel, Nickel, Dime, Dime)
  val expected: Seq[Boolean] = getExpectedResults(inputs)

  // Create the actual hardware test
  dut.io.nickel := false.B
  dut.io.dime := false.B

  for (i <- 0 until 10) {
    when (cycle === i.U) {
      inputs(i) match {
        case NoCoin => // do nothing
        case Nickel => dut.io.nickel := true.B
        case Dime => dut.io.dime := true.B
      }
      assert(dut.io.valid === expected(i).B)
    }
  }
}

class VendingMachineSpec extends CookbookSpec {
  behavior of "VendingMachine"

  it should "work with an explicit FSM" in {
    assertTesterPasses { new VendingMachineTester(new VendingMachineFSM) }
  }

  it should "work without an explicit FSM" in {
    assertTesterPasses { new VendingMachineTester(new VendingMachineNoFSM) }
  }
}
