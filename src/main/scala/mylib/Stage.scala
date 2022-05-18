package mylib

import spinal.core._
import scala.collection.mutable.{HashMap, ListBuffer}


class Stageble[T <: Data](_dataType : => T) extends HardType[T](_dataType) {
  def name=this.getClass.getSimpleName.replace("$","")

  def defaultVal={
    B(0).asInstanceOf[T]
  }
}

// TODO
// edit thess below, to generate the objects by factory methods
object SRC1_VAL extends Stageble(Bits(Config.XLEN))
object SRC2_VAL extends Stageble(Bits(Config.XLEN))

object SRC1 extends Stageble(Bits(5 bits)){
  def range = 19 downto 15
}
object SRC2 extends Stageble(Bits(5 bits)){
  def range = 24 downto 20
}

object DEST extends Stageble(Bits(5 bits)){
  def range = 11 downto 7
}

object IMM12 extends Stageble(Bits(12 bits)){
  def range = 31 downto 20
}
object REG_OUT extends Stageble(Bits(Config.XLEN))

object INST extends Stageble(Bits(32 bits))
object OPCODE extends Stageble(Bits(6 bits))


class Stage extends Area{
  /*
    differences between Inputs/Outputs/Inserts
    Outputs and Inputs would flow between stages, and there would be regs generated between output and next stages input
    while Inserts are just variables used by stages
   */
  var Inputs=new HashMap[Stageble[Data],Data]()
  var Outputs=new HashMap[Stageble[Data],Data]()
  var Inserts=new HashMap[Stageble[Data],Data]()


  var OutputRegs=new HashMap[Stageble[Data],Data]()

  var NextStages= new ListBuffer[Stage]()
  var LastStages= new ListBuffer[Stage]()

  def plug[T <: Area](area : T) : T = {area.setCompositeName(this,getName()).reflectNames();area}
  def outsideCondScope[T](that : => T) : T = {
    val body = Component.current.dslBody  // Get the head of the current component symboles tree (AST in other words)
    val ctx = body.push()                           // Now all access to the SpinalHDL API will be append to it (instead of the current context)
    val swapContext = body.swap()         // Empty the symbole tree (but keep a reference to the old content)
    val ret = that                        // Execute the block of code (will be added to the recently empty body)
    ctx.restore()                            // Restore the original context in which this function was called
    swapContext.appendBack()              // append the original symboles tree to the modified body
    ret                                   // return the value returned by that
  }

  def input[T <: Data](key:Stageble[T]):T={
    Inputs.getOrElseUpdate(key.asInstanceOf[Stageble[Data]],outsideCondScope{
      val input = key()
      //input:=key.defaultVal // we should connect the input to some signals in other places
      // we should set default value, or we will get a latch error
      input.setPartialName(this, key.name)
    }).asInstanceOf[T]
  }

  def insert[T <: Data](key:Stageble[T]):T={
    Inserts.getOrElseUpdate(key.asInstanceOf[Stageble[Data]],outsideCondScope{
      val insert = key()
      insert := key.defaultVal
      insert.setPartialName(this, key.name)
    }).asInstanceOf[T]
  }

  def output[T <: Data](key : Stageble[T]) : T = {
    Outputs.getOrElseUpdate(key.asInstanceOf[Stageble[Data]],outsideCondScope{
      /* use outsideCondScope to place the init logic outside of current scope.
      if the signal defined in current scope, then it could only see the vals inside current scope.
      for example:
      when( some_condition  ){
        output(TEST) := True
        io.test1 := True
      }
      if we don't use outsideCondScope, then we will get:
      TEST := True
      if(some_condition){
        io.test1 = 1'b1;
      } */

      val output = key()
      output:=key.defaultVal
      // we should set default value, or we will get a latch error

      output.setPartialName(this, key.name)
    }).asInstanceOf[T]
  }

  def addPipelineOutputSignal[T<: Data](signals:(Stageble[T],Data)*): Unit ={
    for( (key,signal) <- signals ){
      Outputs += key.asInstanceOf[Stageble[Data]]->RegNext(signal)
    }
  }

  def >>(that:Stage)={
    NextStages += that
    that
    /*
    for ((key,signal) <- Outputs){
      that.input(key) := signal
      //that.Inputs += key->signal
    }
     */
  }

  def build()={
    for((key,signal)<-Outputs){
      OutputRegs += key->RegNext(signal).init(B(0))
    }

    for(stage <- NextStages){
      for( (key,signal) <- stage.Inputs){
        val reg=OutputRegs.getOrElse(key,null)
        if(reg != null){
          signal := reg
        }else{
          // the stage(itself) didn't set the signal as output manually
          // so the next stage couldn't find the signal in OutputRegs
          val input_signal= Inputs.getOrElse(key,null)
          if(input_signal!=null){
            // if we find the signal in Inputs, let's directly connect it to OutputRegs
            val new_reg = RegNext(input_signal).init(B(0))
            OutputRegs += key->new_reg
            signal := new_reg
          }

        }
      }
    }
  }
}
