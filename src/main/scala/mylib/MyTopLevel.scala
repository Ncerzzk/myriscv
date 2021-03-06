/*
 * SpinalHDL
 * Copyright (c) Dolu, All rights reserved.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */

package mylib


import spinal.core._
import spinal.lib._

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.util.Random

// Code Style
// Class Name : Pascal(Upper Camel)
// Method/Attribute : Camel
// Local variable : Under Score

object InstructionOPCode extends SpinalEnum{
  val NOP,SLL,SRL,ADD,SLT,AND,OR,XOR,SUB,SRA = newElement()
  val ADDI=newElement()
  val JALR=newElement()
}

class InstBundle(val inst:MaskedLiteral,val opcode:InstructionOPCode.C,decode_action: => Unit,execute_action: => Unit){
  // this class is used for compack inst with its opcode and actions together
  def decode=decode_action
  def execute=execute_action

  val careAbout=inst.careAbout
  val value=inst.value
}

class CPUPlugin(cpu:CPU){
  def decodeActionWith_RD_RS1(opcode:InstructionOPCode.C):Unit={
    import cpu.ID._
    output(OPCODE) := opcode.asBits.resized

//    insert(SRC1) :=  input(INST)(SRC1.range)
//    output(SRC1_VAL) := Mux(cpu.EX.input(DEST)===insert(SRC1),
//      whenTrue=cpu.EX.output(REG_OUT),
//      whenFalse=cpu.regfile.read(insert(SRC1).asUInt)
//    )
    output(SRC1_VAL) := insert(SRC1_VAL)
    output(DEST) := input(INST)(DEST.range)
  }
  def decodeActionWith_RD_RS1_RS2(opcode:InstructionOPCode.C): Unit ={
    import cpu.ID._

    decodeActionWith_RD_RS1(opcode)
//    insert(SRC2) :=  input(INST)(SRC2.range)
//    output(SRC2_VAL) := Mux(cpu.EX.input(DEST)===insert(SRC2),
//      whenTrue=cpu.EX.output(REG_OUT),
//      whenFalse=cpu.regfile.read(insert(SRC2).asUInt)
//    )
    output(SRC2_VAL) := insert(SRC2_VAL)

  }

  def decodeActionWith_RD_RS1_IMM12(opcode:InstructionOPCode.C): Unit ={
    import cpu.ID._
    decodeActionWith_RD_RS1(opcode)
    output(SRC2_VAL) := input(INST)(IMM12.range).asSInt.resize(SRC2_VAL.getBitsWidth).asBits
    // let 's use sign expand here
  }

  def executeActionWith_RD_2SRC(block_get_result:(Bits,Bits) => Bits)= {
    import cpu.EX._
    output(REG_OUT) := block_get_result(input(SRC1_VAL),input(SRC2_VAL))
    //executeActionWriteRD(insert(REG_OUT))
  }
  def executeActionWriteRD(rd_val:Bits)={
    import cpu.EX._
    //cpu.regfile.write(input(DEST).asUInt,rd_val)
    output(REG_OUT) := rd_val
    //insert(REG_OUT) := rd_val
  }
}

class Shifter(val cpu:CPU) extends CPUPlugin(cpu){
  val SLL = new InstBundle(
    inst=Instructions.SLL,
    opcode=InstructionOPCode.SLL,
    decode_action = decodeActionWith_RD_RS1_RS2(InstructionOPCode.SLL),
    execute_action = executeActionWith_RD_2SRC((src1,src2)=> src1 |<< src2.asUInt)
  )
  val ADD = new InstBundle(
    inst=Instructions.ADD,
    opcode=InstructionOPCode.ADD,
    decode_action = decodeActionWith_RD_RS1_RS2(InstructionOPCode.ADD),
    execute_action = executeActionWith_RD_2SRC((src1,src2)=> (src1.asUInt + src2.asUInt).asBits)
  )
  val ADDI = new InstBundle(
    inst=Instructions.ADDI,
    opcode=InstructionOPCode.ADDI,
    decode_action = decodeActionWith_RD_RS1_IMM12(InstructionOPCode.ADDI),
    execute_action = executeActionWith_RD_2SRC((src1,src2)=> (src1.asUInt + src2.asUInt).asBits)
  )

  val JALR = new InstBundle(
    inst=Instructions.JALR,
    opcode=InstructionOPCode.JALR,
    decode_action = decodeActionWith_RD_RS1_IMM12(InstructionOPCode.JALR),
    execute_action = {

      val target_address=cpu.EX.input(SRC1_VAL).asSInt + cpu.EX.input(SRC2_VAL).asSInt
      cpu.PC.write((target_address.takeHigh(31) ## B("1'b0")).asUInt)
      val link_address=(cpu.EX.input(PC_VAL).asUInt + 4)
      executeActionWriteRD(link_address.asBits)
      cpu.EX.insert(FLUSH) := True
      //cpu.EX.insert(REG_OUT) := link_address.asBits
    }
  )
  val insts=List(SLL,ADD,ADDI,JALR)

  def build(): Unit ={
    for(i<- insts){
      cpu.ID.addDecode(i)

      cpu.EX.plug(new Area{
        import cpu.EX._
        when(input(OPCODE)===i.opcode.asBits.resized){
          i.execute
        }
      })
    }
  }

  def finalBuild():Unit={
    // called after interconnection finished

    // add stage regs flush support
    cpu.ID.plug(
      new Area{
        when(cpu.EX.insert(FLUSH)){
          cpu.ID.getOutputReg(OPCODE) := B(0)
        }
      }
    )
    cpu.IF.plug(
      new Area{
        when(cpu.EX.insert(FLUSH)){
          cpu.IF.getOutputReg(INST) := B(0)
        }
      }
    )
  }

}

case class SOCInitData(rom_init_array:Seq[String],regfile_init_array:Seq[Int])

class SOC(init_data:SOCInitData=null) extends Component{
  val rom = new ROM
  val cpu = new CPU
  val ram = new TCM(Config.RAM_Size,true)

  rom.io <> cpu.io.rom_interface
  ram.io <> cpu.io.ram_interface

  //rom.init(List("sll x3,x2,x1","add x2,x1,x0","sll x3,x2,x1"))
  if (init_data!=null){
    rom.init(init_data.rom_init_array)
    cpu.regfile.init(List(0,1,2,3,4,5,6))
  }
  //cpu.regfile.init(List(B(0),B(1),B(2),B(3)))
}

object SOC{
  def apply(rom_init_array:Seq[String],regfile_init_array:Seq[Int]) ={
    val initData=SOCInitData(rom_init_array, regfile_init_array)
    new SOC(initData)
  }

  def apply=new SOC
}

//Generate the MyTopLevel's Verilog
object MyTopLevelVerilog {
  def main(args: Array[String]) {
    SpinalVerilog(new SOC).printPruned()
  }
}