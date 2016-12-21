// See LICENSE for license details.

package chiselTests

import chisel3._
import chisel3.util._
import chisel3.testers.BasicTester

// IO for Modules that just connect bus to out
class InoutReaderIO extends Bundle {
  val bus = Analog(32.W)
  val out = Output(UInt(32.W))
}
// IO for Modules that drive bus from in (there should be only 1)
class InoutWriterIO extends Bundle {
  val bus = Analog(32.W)
  val in = Input(UInt(32.W))
}

trait InoutReader {
  self: Module =>
  final val io = self.IO(new InoutReaderIO)
}

class InoutReaderBlackBox extends BlackBox with InoutReader

class InoutReaderWrapper extends Module with InoutReader {
  val mod = Module(new InoutReaderBlackBox)
  io <> mod.io
}
class InoutWriterBlackBox extends BlackBox {
  val io = IO(new InoutWriterIO)
}
// Connects two Analog ports
class InoutConnector extends Module {
  val io = IO(new Bundle {
    val bus1 = Input(Analog(32.W))
    val bus2 = Input(Analog(32.W))
  })
  io.bus1 <> io.bus2
}

// Parent class for tests connecing up InoutReaders and InoutWriters
abstract class InoutTester extends BasicTester {
  final val BusValue = "hdeadbeef".U

  final val (cycle, done) = Counter(true.B, 2)
  when (done) { stop(); stop() } // Double stop for Verilator

  final val writer = Module(new InoutWriterBlackBox)
  writer.io.in := BusValue

  final def check(reader: Module with InoutReader): Unit =
    assert(reader.io.out === BusValue)
}

class InoutSpec extends ChiselFlatSpec {
  behavior of "Analog stitching"

  it should "work with 2 blackboxes bulk connected" in {
    assertTesterPasses(new InoutTester {
      val mod = Module(new InoutReaderBlackBox)
      mod.io.bus <> writer.io.bus
      check(mod)
    }, Seq("/InoutBlackBox.v"))
  }
  it should "work with 3 blackboxes connected via wire" in {
    assertTesterPasses(new InoutTester {
      val mods = Seq.fill(2)(Module(new InoutReaderBlackBox))
      val busWire = Wire(Analog(32.W))
      busWire <> writer.io.bus
      busWire <> mods(0).io.bus
      mods(1).io.bus <> busWire // check other bulk connect
      mods.foreach(check(_))
    }, Seq("/InoutBlackBox.v"))
  }
  // TODO This fails in Firrtl, should this be supported?
  ignore should "allow Analog wires to be connected" in {
    assertTesterPasses(new InoutTester {
      val mod = Module(new InoutReaderBlackBox)
      val busWire = Seq.fill(2)(Wire(Analog(32.W)))
      busWire(0) <> writer.io.bus
      busWire(1) <> mod.io.bus
      busWire(0) <> busWire(1)
      check(mod)
    }, Seq("/InoutBlackBox.v"))
  }
  // TODO This should probably actually fail during elaboration rather than during simulation!
  it should "respect last connect semantics" in {
    assertTesterFails(new InoutTester {
      val mods = Seq.fill(2)(Module(new InoutReaderBlackBox))
      writer.io.bus <> mods(0).io.bus // This connection gets overruled!
      writer.io.bus <> mods(1).io.bus
      mods.foreach(check(_))
    }, Seq("/InoutBlackBox.v"))
  }
  it should "work with blackboxes at different levels of the module hierarchy" in {
    assertTesterPasses(new InoutTester {
      val mods = Seq(Module(new InoutReaderBlackBox), Module(new InoutReaderWrapper))
      val busWire = Wire(writer.io.bus)
      busWire <> writer.io.bus
      busWire <> mods(0).io.bus
      busWire <> mods(1).io.bus
      mods.foreach(check(_))
    }, Seq("/InoutBlackBox.v"))
  }
  // TODO This is not supported in Firrtl, should this be supported?
  it should "NOT SUPPORT two analog ports in the same module" in {
    assertTesterFails(new InoutTester {
      val reader = Module(new InoutReaderBlackBox)
      val connector = Module(new InoutConnector)
      connector.io.bus1 <> writer.io.bus
      reader.io.bus <> connector.io.bus2
      check(reader)
    }, Seq("/InoutBlackBox.v"))
  }
  // TODO What should happen here?
  // This creates a wire in order to connect the Analog types
  // Since it is created within the when scope the connection is actually unconditional
  // So the Tester passes
  ignore should "NOT SUPPORT conditional connection of analog types" in {
    a [ChiselException] should be thrownBy {
      assertTesterPasses(new InoutTester {
        val mod = Module(new InoutReaderBlackBox)
        when (cycle > 3.U) {
          mod.io.bus <> writer.io.bus
        }
        check(mod)
      }, Seq("/InoutBlackBox.v"))
    }
  }
}
