package mylib
import spinal.core._

import scala.collection.mutable.HashMap
import scala.util.matching.Regex


object Instructions {
  def SLLI_RV32 = M"0000000----------001-----0010011"

  def SRLI_RV32 = M"0000000----------101-----0010011"

  def SRAI_RV32 = M"0100000----------101-----0010011"

  def FRFLAGS = M"00000000000100000010-----1110011"

  def FSFLAGS = M"000000000001-----001-----1110011"

  def FSFLAGSI = M"000000000001-----101-----1110011"

  def FRRM = M"00000000001000000010-----1110011"

  def FSRM = M"000000000010-----001-----1110011"

  def FSRMI = M"000000000010-----101-----1110011"

  def FSCSR = M"000000000011-----001-----1110011"

  def FRCSR = M"00000000001100000010-----1110011"

  def RDCYCLE = M"11000000000000000010-----1110011"

  def RDTIME = M"11000000000100000010-----1110011"

  def RDINSTRET = M"11000000001000000010-----1110011"

  def RDCYCLEH = M"11001000000000000010-----1110011"

  def RDTIMEH = M"11001000000100000010-----1110011"

  def RDINSTRETH = M"11001000001000000010-----1110011"

  def SCALL = M"00000000000000000000000001110011"

  def SBREAK = M"00000000000100000000000001110011"

  def FMV_X_S = M"111000000000-----000-----1010011"

  def FMV_S_X = M"111100000000-----000-----1010011"

  def FENCE_TSO = M"100000110011-----000-----0001111"

  def PAUSE = M"00000001000000000000000000001111"

  def BEQ = M"-----------------000-----1100011"

  def BNE = M"-----------------001-----1100011"

  def BLT = M"-----------------100-----1100011"

  def BGE = M"-----------------101-----1100011"

  def BLTU = M"-----------------110-----1100011"

  def BGEU = M"-----------------111-----1100011"

  def JALR = M"-----------------000-----1100111"

  def JAL = M"-------------------------1101111"

  def LUI = M"-------------------------0110111"

  def AUIPC = M"-------------------------0010111"

  def ADDI = M"-----------------000-----0010011"

  def SLTI = M"-----------------010-----0010011"

  def SLTIU = M"-----------------011-----0010011"

  def XORI = M"-----------------100-----0010011"

  def ORI = M"-----------------110-----0010011"

  def ANDI = M"-----------------111-----0010011"

  def ADD = M"0000000----------000-----0110011"

  def SUB = M"0100000----------000-----0110011"

  def SLL = M"0000000----------001-----0110011"

  def SLT = M"0000000----------010-----0110011"

  def SLTU = M"0000000----------011-----0110011"

  def XOR = M"0000000----------100-----0110011"

  def SRL = M"0000000----------101-----0110011"

  def SRA = M"0100000----------101-----0110011"

  def OR = M"0000000----------110-----0110011"

  def AND = M"0000000----------111-----0110011"

  def LB = M"-----------------000-----0000011"

  def LH = M"-----------------001-----0000011"
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
  args_range += "rd"->(11,7)
  args_range += "rt"->(19,15)
  args_range += "rs1"->(19,15)
  args_range += "rs2"->(24,20)
  args_range += "rs3"->(31,27)
  args_range += "imm20"->(31,12)
  args_range += "jimm20"->(31,12)
  args_range += "imm12"->(31,20)
  args_range += "imm12hi"->(31,25)

  def instsWith2RS(inst:String)=inst+" rd,rs1,rs2"
  def instsWithIMM12(inst:String)=inst+" rd,rs1,imm12"

  val instsWith2RSList=List("add","sll","sub","slt","sltu","xor","srl","sra","or","and")
  val instsWithIMM12List=List("lb","lh","lw","lbu","lhu","addi","slti","sltiu","xori","ori","andi","jalr")

  for (i <- instsWith2RSList){
    insts += i->instsWith2RS(i)
  }

  for (i <- instsWithIMM12List){
    insts += i->instsWithIMM12(i)
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
      val pattern="0[xX](\\d+)".r()
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
    val inst_list=new InstTemplate(inst).list  // add x1,x2,x0
    val inst_name=inst_list.head  // x1
    val inst_template = insts(inst_name) //
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
  }

}