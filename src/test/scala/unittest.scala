import org.scalatest
import mylib._
object unittest {
  def main(args: Array[String]): Unit ={
    testInstTemplate()
    testASM()
  }

  def testInstTemplate(): Unit ={
    def getInstList(str:String)=new InstTemplate(str).list
    assert(getInstList("add rd,rs1,rs2")==getInstList("add rd, rs1, rs2"))
    assert(getInstList("jal rd").length==2)
  }

  def testASM():Unit={
    assert(CompileInstructions.asm("add 1,2,3")=="00000000001100010000000010110011")
    assert(CompileInstructions.asm("add 0x1,0x2,0x3")=="00000000001100010000000010110011")
  }
}
