package mylib

import spinal.core._
import spinal.sim._
import spinal.core.sim._

import scala.util.Random


//MyTopLevel's testbench
object MySpinalConfig extends SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC))

object MyTopLevelSim {
  def main(args: Array[String]) {
    SimConfig.withWave.withConfig(MySpinalConfig).doSim(new SOC ){dut =>
      //Fork a process to generate the reset and the clock on the dut
      dut.clockDomain.forkStimulus(period = 2)
      fork{
        for(i<- 0 to 99){
          dut.clockDomain.waitRisingEdge()
        }
      }.join()
    }
  }
}