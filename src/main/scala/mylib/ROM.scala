package mylib

import spinal.core._
import spinal.lib._

import scala.collection.Seq

object ROM{
  def interface=new Bundle with IMasterSlave{
    val addr =  UInt(log2Up(Config.ROM_Size) bits)
    val data =  Bits(32 bits)

    override def asMaster(): Unit = {
      addr.asInput()
      data.asOutput()
    }
  }

  def interfaceOfSelf=master(interface)
  def interfaceOfCPU=slave(interface)
}

class ROM extends Component{
  // size: bytes
  // len : size/4

  val testio= new Bundle{
    val io=out(True)
  }

  val len = Config.ROM_Size/4
  val io = ROM.interfaceOfSelf
  val rom=Mem(Bits(32 bits),len).init(Array.fill(len)(B(0)))

  io.data := rom.readSync(io.addr>>2)

  def init(initData:Seq[String]):Unit={
    assert (initData.length < len, "error")
    val init_array = Array.fill(len)(B(0))
    for(index <- 0 until initData.length){
      val str=initData(index)
      init_array(index) = B("32'b"+CompileInstructions.asm(str))
    }
    rom.init(init_array)
  }

}
