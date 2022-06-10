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

object TCM{
  def interface(size:Int, write:Boolean)=new Bundle with IMasterSlave{
    val addr = UInt(log2Up(size) bits)
    val rddata = Bits(Config.XLEN)
    var wddata:Bits=null
    var write_en:Bool=null
    if(write){
      wddata=Bits(Config.XLEN).setPartialName("wddata")
      write_en=Bool().setPartialName("write_en")
    }


    override def asMaster(): Unit = {
      addr.asInput()
      rddata.asOutput()
      if(write){
        wddata.asInput()
        write_en.asInput()
      }

    }

  }

  def interfaceOfSelf(size:Int,write:Boolean)=master(interface(size,write))
  def interfaceOfCPU(size:Int,write:Boolean)=slave(interface(size,write))
}
class TCM(val size:Int,write:Boolean) extends Component{
  val len = size / 4
  val io = TCM.interfaceOfSelf(size,write)
  val mem= Mem(Bits(32 bits),len).init(Array.fill(len)(B(0)))

  val index=io.addr.takeHigh(io.addr.getBitsWidth -2).asUInt
  if(write){
    io.rddata := mem.readWriteSync(index,io.wddata,True,io.write_en)
  }else{
    io.rddata := mem.readSync(index)
  }

}

class ROM extends Component{
  // size: bytes
  // len : size/4

  val len = Config.ROM_Size/4
  val io = ROM.interfaceOfSelf
  val rom=Mem(Bits(32 bits),len).init(Array.fill(len)(B(0)))

  io.data := rom.readSync(io.addr.takeHigh(io.addr.getBitsWidth -2).asUInt)

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
