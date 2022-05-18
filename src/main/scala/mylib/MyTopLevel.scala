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
  val NOP,SLL,ADD = newElement()
}

class CPUPlugin(cpu:CPU){
  class InstBundle(inst:MaskedLiteral,opcode:InstructionOPCode.C,decode_action: => Unit,execute_action: => Unit){
    def decode=decode_action
    def execute=execute_action
  }

  def decodeActionWith_RD_RS1_RS2(opcode:InstructionOPCode.C): Unit ={
    import cpu.ID._
    output(OPCODE) := opcode.asBits.resized
    output(SRC1) := cpu.regfile.read(input(INST)(19 downto 15).asUInt)
    output(SRC2) := cpu.regfile.read(input(INST)(24 downto 20).asUInt)
    output(DEST) := input(INST)(11 downto 7)
  }

  def decodeActionWith_RD_RS1_IMM12(opcode:InstructionOPCode.C): Unit ={
    import cpu.ID._
    output(OPCODE) := opcode.asBits.resized
    output(SRC1) := cpu.regfile.read(input(INST)(SRC1.range).asUInt)
    output(SRC2) := input(INST)(IMM12.range)
    output(DEST) := input(INST)(DEST.range)
  }

  def executeActionWriteRD(block_get_result:(Bits,Bits) => Bits)={
    import cpu.EX._
    cpu.regfile.write(input(DEST).asUInt, block_get_result(input(SRC1),input(SRC2)))
  }
}

class Shifter(cpu:CPU) extends CPUPlugin(cpu){
  val SLL = new InstBundle(
    inst=Instructions.SLL,
    opcode=InstructionOPCode.SLL,
    decode_action = decodeActionWith_RD_RS1_RS2(InstructionOPCode.SLL),
    execute_action = executeActionWriteRD((src1,src2)=> src1 |<< src2.asUInt)
  )
  val insts=List(SLL
  )

  def build(cpu:CPU): Unit ={
    cpu.ID.plug(new Area{
      import cpu.ID._
      when(input(INST)===Instructions.SLL){
        SLL.decode
      }
    })

    cpu.EX.plug(new Area{
      import cpu.EX._
      when(input(OPCODE)===InstructionOPCode.SLL.asBits.resized){
        SLL.execute
      }
    })
  }
}

case class SOCInitData(rom_init_array:Seq[String],regfile_init_array:Seq[Int])

class SOC(init_data:SOCInitData=null) extends Component{
  val rom = new ROM
  val cpu = new CPU

  rom.io <> cpu.io.rom_interface

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