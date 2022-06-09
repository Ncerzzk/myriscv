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
        List("sll x3,x2,x1","add x2,x1,x0","sll x3,x2,x1","addi x4,x2,100","addi x3,x2,-1","jalr x5,x4,100"),
        List(0,1,2,3,4,5)
      )
    }
    compile.doSim("hello"){dut =>

      //Fork a process to generate the reset and the clock on the dut
      dut.clockDomain.forkStimulus(period = 2)
      fork{
        dut.clockDomain.waitSampling(100)
      }.join()
    }
  }
}