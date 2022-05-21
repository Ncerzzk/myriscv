package mylib

import spinal.core._

import scala.collection.mutable.{ListBuffer,HashMap}

class RegFile(val cpu:CPU) extends Area{
  val regs=Mem(Bits(Config.XLEN),32).init(Array.fill(32)(B(0)))
  //def read(reg_addr:UInt)=regs.readAsync(reg_addr,writeFirst)
  def read(reg_addr:UInt)=regs.readSync(reg_addr,null)
  def write(reg_addr:UInt,data:Bits,condition:Bool=null)=regs.write(reg_addr,data,condition)


  def init(initData:Seq[Int]):Unit={
    assert (initData.length < 32, "error")
    val empty_array = Array.fill(32-initData.length)(B(0))
    regs.init(initData.map(x=>B(x)).toList++empty_array)
  }



  def build(): Unit ={
    cpu.ID.plug(new Area{
      import cpu.ID._
      output(SRC1,false) :=  input(INST)(SRC1.range)
      output(SRC2,false) :=  input(INST)(SRC2.range)
    })

    val src1RegOut=read(cpu.ID.output(SRC1).asUInt)
    val src2RegOut=read(cpu.ID.output(SRC2).asUInt)

    cpu.EX.plug(new Area{
      switch(cpu.EX.input(SRC1)){
        is(cpu.WB.input(DEST)){
          cpu.EX.input(SRC1_VAL) := cpu.WB.output(REG_OUT,false)
        }
        is(cpu.LD.input(DEST)){
          cpu.EX.input(SRC1_VAL) := cpu.LD.output(REG_OUT,false)
        }
        default{
          cpu.EX.input(SRC1_VAL) := src1RegOut
        }
      }

      switch(cpu.EX.input(SRC2)){
        is(cpu.WB.input(DEST)){
          cpu.EX.input(SRC2_VAL) := cpu.WB.output(REG_OUT,false)
        }
        is(cpu.LD.input(DEST)){
          cpu.EX.input(SRC2_VAL) := cpu.LD.output(REG_OUT,false)
        }
        default{
          cpu.EX.input(SRC2_VAL) := src2RegOut
        }
      }
    })
  }
}

class CPU extends Component{

  val io = new Bundle{
    val rom_interface=ROM.interfaceOfCPU
    val test= out(True)
  }

  val regfile=new RegFile(this)

  val PC = new Area{
    val reg = Reg(UInt(Config.XLEN)).init(U(0))
    def read=reg
    def write(target:UInt)= reg:=target

    reg := reg+4
  }

  io.rom_interface.addr := PC.read.takeLow(9).asUInt

  val IF = new Stage{
    input(INST) := io.rom_interface.data
    output(PC_VAL,false) := PC.read.asBits
  }
  val EX = new Stage
  val ID = new Stage{
    val careList = HashMap[BigInt,ListBuffer[InstBundle]]()
    def addDecode(inst_bundle:InstBundle)={
      careList.getOrElseUpdate(inst_bundle.careAbout,ListBuffer[InstBundle]()) += inst_bundle
    }
    override def build(): Unit = {
      //bypassBuild()

      for(i <- careList){
        val care_bits = input(INST)&i._1
        for(j <- i._2){
          when(care_bits === j.value){
            j.decode
          }
        }
      }
      super.build()
    }

    def addBypass(stage:Stage)={
      when(insert(SRC1) === stage.input(DEST)){
        insert(SRC1_VAL) := stage.output(REG_OUT,false)
      }
      when(insert(SRC2) === stage.input(DEST)){
        insert(SRC2_VAL) := stage.output(REG_OUT,false)
      }
    }

    def bypassBuild(): Unit ={
      this.plug(new Area{
        insert(SRC1,false) :=  input(INST)(SRC1.range)
        insert(SRC2,false) :=  input(INST)(SRC2.range)
        insert(SRC1_VAL,false) := regfile.read(insert(SRC1).asUInt)
        insert(SRC2_VAL,false) := regfile.read(insert(SRC2).asUInt)

        addBypass(EX)
        addBypass(LD)
        addBypass(WB)
      })
    }
  }

  val LD = new Stage
  val WB = new Stage{
    regfile.write(input(DEST).asUInt,input(REG_OUT),input(DEST)=/=B(0))
  }
  val stageList=List(IF,ID,EX,LD,WB)

  IF >> ID >> EX >> LD>> WB

  val a=new Shifter(this)
  a.build()  // plugins build

  regfile.build()

  for(i<- stageList){   // stages build
    i.build()
  }

  for(i<- stageList){
    i.inferConnections()
  }

  a.finalBuild()
}
