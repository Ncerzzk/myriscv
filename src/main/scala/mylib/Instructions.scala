package mylib
import spinal.core._

import scala.collection.mutable.HashMap
import scala.util.matching.Regex

object Instructions {
  def BEQ                = M"-----------------000-----1100011"
  def BNE                = M"-----------------001-----1100011"
  def BLT                = M"-----------------100-----1100011"
  def BGE                = M"-----------------101-----1100011"
  def BLTU               = M"-----------------110-----1100011"
  def BGEU               = M"-----------------111-----1100011"
  def JALR               = M"-----------------000-----1100111"
  def JAL                = M"-------------------------1101111"
  def LUI                = M"-------------------------0110111"
  def AUIPC              = M"-------------------------0010111"
  def ADDI               = M"-----------------000-----0010011"
  def SLTI               = M"-----------------010-----0010011"
  def SLTIU              = M"-----------------011-----0010011"
  def XORI               = M"-----------------100-----0010011"
  def ORI                = M"-----------------110-----0010011"
  def ANDI               = M"-----------------111-----0010011"
  def ADD                = M"0000000----------000-----0110011"
  def SUB                = M"0100000----------000-----0110011"
  def SLL                = M"0000000----------001-----0110011"
  def SLT                = M"0000000----------010-----0110011"
  def SLTU               = M"0000000----------011-----0110011"
  def XOR                = M"0000000----------100-----0110011"
  def SRL                = M"0000000----------101-----0110011"
  def SRA                = M"0100000----------101-----0110011"
  def OR                 = M"0000000----------110-----0110011"
  def AND                = M"0000000----------111-----0110011"
  def LB                 = M"-----------------000-----0000011"
  def LH                 = M"-----------------001-----0000011"
  def LW                 = M"-----------------010-----0000011"
  def LBU                = M"-----------------100-----0000011"
  def LHU                = M"-----------------101-----0000011"
  def SB                 = M"-----------------000-----0100011"
  def SH                 = M"-----------------001-----0100011"
  def SW                 = M"-----------------010-----0100011"
  def FENCE              = M"-----------------000-----0001111"
  def FENCE_I            = M"-----------------001-----0001111"
  def MUL                = M"0000001----------000-----0110011"
  def MULH               = M"0000001----------001-----0110011"
  def MULHSU             = M"0000001----------010-----0110011"
  def MULHU              = M"0000001----------011-----0110011"
  def DIV                = M"0000001----------100-----0110011"
  def DIVU               = M"0000001----------101-----0110011"
  def REM                = M"0000001----------110-----0110011"
  def REMU               = M"0000001----------111-----0110011"
}


class InstTemplate(template:String){
  //val a = new InstTemplate("add    rd,   rs1  ,   rs2")
  //val pattern="(\\w+)\\s+(\\w+)\\s*(?:,\\s*(\\w+)\\s*)*".r()
  val pattern=(
    "(\\w+)\\s+(\\w+)\\s*" +
    ",\\s*([\\w-]+)\\s*".repeat(template.count(_==','))
    ).r()
  val list=pattern.findFirstMatchIn(template) match {
    case Some(x) => x.subgroups
    case _ => List()
  }
}

object CompileInstructions{
  implicit def test(aaa:(Int,Int))=Range.inclusive(aaa._2,aaa._1)
  implicit def stringToInstTemplate(str:String)=new InstTemplate(str)

  var args_range=new HashMap[String,Range]()
  var insts= new HashMap[String,InstTemplate]()
  var preAsmInsts= new HashMap[String,String=>String]()
  args_range += "rd"->(11,7)
  args_range += "rt"->(19,15)
  args_range += "rs1"->(19,15)
  args_range += "rs2"->(24,20)
  args_range += "rs3"->(31,27)
  args_range += "imm20"->(31,12)
  args_range += "jimm20"->(31,12)
  args_range += "imm12"->(31,20)
  args_range += "imm12hi"->(31,25)

  args_range += "imm12lo"->(11,7)
  args_range += "imm12hi"->(31,25)

  def instsWith2RS(inst:String)=inst+" rd,rs1,rs2"
  def instsWithIMM12(inst:String)=inst+" rd,rs1,imm12"
  def instsWithIMM12S(inst:String)=inst+" rs1,rs2,imm12hi,imm12lo"

  def storePreAsm(inst:String)= {
    val imm_str=inst.split(",").last.strip()
    val imm = parseStr2Int(imm_str)
    val high_bits = imm >> 5
    val low_bits = imm & 0x1F
    val new_imm_str = high_bits.toString + "," + low_bits.toString

    inst.replace(imm_str,new_imm_str)
  }

  preAsmInsts += "sb" -> storePreAsm
  preAsmInsts += "sh" -> storePreAsm
  preAsmInsts += "sw" -> storePreAsm

  val instsWith2RSList=List("add","sll","sub","slt","sltu","xor","srl","sra","or","and")
  val instsWithIMM12List=List("lb","lh","lw","lbu","lhu","addi","slti","sltiu","xori","ori","andi","jalr")
  val instsWithIMM12SList=List("sb","sh","sw")

  for (i <- instsWith2RSList){
    insts += i->instsWith2RS(i)
  }

  for (i <- instsWithIMM12List){
    insts += i->instsWithIMM12(i)
  }

  for(i <- instsWithIMM12SList){
    insts += i->instsWithIMM12S(i)
  }

  def replaceFun(string: String, startIndex: Int, endIndex: Int, replacement:String) = {

    // Get the first part
    val a = string.substring(0, startIndex)

    // Get the middle part and do your replacing
    val b = replacement

    // Get the last part
    val c = string.substring(endIndex + 1)

    // Put it all together and return
    a + b + c
  }
  def parseStr2Int(str:String): Int ={
    if(str.contains("0x") || str.contains("0X")){
      val pattern="0[xX]([\\dABCDEFabcdef]+)".r()
      Integer.parseInt(pattern.findFirstMatchIn(str) match {
        case Some(data) => data.group(1)
        case _ => "None"
      },16)
    }else if(str.contains("x")){
      // handle registers x0,x1 ... etc.
      val pattern="[xX](\\d+)".r()
      Integer.parseInt(pattern.findFirstMatchIn(str) match {
        case Some(data) => data.group(1)
        case _ => "None"
      },10)
    } else{
      Integer.parseInt(str,10)
    }
  }
  def asm(inst:String):String={

    var inst_list=new InstTemplate(inst).list  // add x1,x2,x0
    val inst_name=inst_list.head  // add

    val preAsmFunc=preAsmInsts.getOrElse(inst_name,null)
    if(preAsmFunc != null){
      inst_list=new InstTemplate(preAsmFunc(inst)).list
    }

    val inst_template = insts(inst_name) // use add to find the template
    var inst_markstr=Instructions.getClass.getMethod(inst_name.toUpperCase)
      .invoke(Instructions).asInstanceOf[MaskedLiteral].getBitsString(32,'-')
    for (i <- 1 until inst_template.list.length){
      val arg_name=inst_template.list(i)
      val arg_range=args_range(arg_name)
      var arg_val= parseStr2Int(inst_list(i)).toBinaryString
      if(arg_val.length>arg_range.length){
        // when the val <0
        arg_val=arg_val.substring(arg_val.length-arg_range.length)
      }else{
        arg_val = "0".repeat(arg_range.length-arg_val.length)+arg_val
      }

      inst_markstr=replaceFun(inst_markstr,32-arg_range.end-1,32-arg_range.start-1,arg_val)
    }
    inst_markstr
  }

  def main(a:Array[String]): Unit ={
    asm("add 1,2,3")
    println(asm("sb 1,2,0xC23"))
  }

}