package mylib

import spinal.core._
import spinal.sim._
import spinal.core.sim._

import scala.util.Random


//MyTopLevel's testbench
object MySpinalConfig extends SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC))

object MyTopLevelSim {
  def main(args: Array[String]) {
    val compile=SimConfig.withWave.withConfig(MySpinalConfig).withVerilator.compile {
      SOC(
        List("sll x3,x2,x1","add x2,x1,x0","sll x3,x2,x1"),
        List(0,1,2,3,4,5)
      )
    }
    compile.doSim("hello"){dut =>

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