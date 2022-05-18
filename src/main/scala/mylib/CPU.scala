package mylib

import spinal.core._

class RegFile extends Area{
  val regs=Mem(Bits(Config.XLEN),32).init(Array.fill(32)(B(0)))
  def read(reg_addr:UInt)=regs.readAsync(reg_addr,writeFirst)
  def write(reg_addr:UInt,data:Bits)=regs.write(reg_addr,data)

  def init(initData:Seq[Int]):Unit={
    assert (initData.length < 32, "error")
    val empty_array = Array.fill(32-initData.length)(B(0))
    regs.init(initData.map(x=>B(x)).toList++empty_array)
  }
}

class CPU extends Component{

  val io = new Bundle{
    val rom_interface=ROM.interfaceOfCPU
  }
  val regfile=new RegFile

  val PC = new Area{
    val reg = Reg(UInt(Config.XLEN)).init(U(0))
    def read=reg

    reg := reg+4
  }

  io.rom_interface.addr := PC.read.takeLow(9).asUInt

  val IF = new Stage{
    input(INST) := io.rom_interface.data
  }
  val ID = new Stage
  val EX = new Stage
  val WB = new Stage

  IF >> ID >> EX >>WB

  val a=new Shifter(this)
  a.build(this)

  IF.build()
  ID.build()


}
