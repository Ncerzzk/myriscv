package mylib

import spinal.core._

import scala.collection.mutable.{HashMap, ListBuffer}


class Stageble[T <: Data](_dataType : => T) extends HardType[T](_dataType){
  def name=this.getClass.getSimpleName.replace("$","")

  def defaultVal:T={
    _dataType match{
      case _:Bool => False.asInstanceOf[T]
      case _ => B(0).asInstanceOf[T]
    }
    //B(0).asInstanceOf[T]
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
object PC_VAL extends Stageble(Bits(Config.XLEN))
object INST extends Stageble(Bits(32 bits))
object OPCODE extends Stageble(Bits(6 bits))
object FLUSH extends Stageble(Bool)

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

  def getSignalAndInit[T <: Data](hash: HashMap[Stageble[Data], Data],key:Stageble[T],init:Boolean,prefix:String): T ={
    hash.getOrElseUpdate(key.asInstanceOf[Stageble[Data]],outsideCondScope{
      val signal = key()
      if(init){
        signal := key.defaultVal
      }
      signal.setPartialName(this,prefix+"_"+key.name)
    }).asInstanceOf[T]
  }

  def input[T <: Data](key:Stageble[T],default_init:Boolean=false):T={
//    Inputs.getOrElseUpdate(key.asInstanceOf[Stageble[Data]],outsideCondScope{
//      val input = key()
//      //input:=key.defaultVal // we should connect the input to some signals in other places
//      // we should set default value, or we will get a latch error
//      if(default_init){
//        input := key.defaultVal
//      }
//      input.setPartialName(this, "input_"+key.name)
//    }).asInstanceOf[T]
    getSignalAndInit(Inputs,key,default_init,"input")
  }

  def insert[T <: Data](key:Stageble[T],default_init:Boolean=true):T={
//    Inserts.getOrElseUpdate(key.asInstanceOf[Stageble[Data]],outsideCondScope{
//      val insert = key()
//      if(default_init){
//        insert := key.defaultVal
//      }
//      insert.setPartialName(this, "INSERT_"+key.name)
//    }).asInstanceOf[T]
    getSignalAndInit(Inserts,key,default_init,"insert")
  }

  def output[T <: Data](key : Stageble[T],default_init:Boolean=true) : T = {
//    Outputs.getOrElseUpdate(key.asInstanceOf[Stageble[Data]],outsideCondScope{
//      /* use outsideCondScope to place the init logic outside of current scope.
//      if the signal defined in current scope, then it could only see the vals inside current scope.
//      for example:
//      when( some_condition  ){
//        output(TEST) := True
//        io.test1 := True
//      }
//      if we don't use outsideCondScope, then we will get:
//      TEST := True
//      if(some_condition){
//        io.test1 = 1'b1;
//      } */
//
//      val output = key()
//      if(default_init){
//        output:=key.defaultVal
//      }
//      // we should set default value, or we will get a latch error
//
//      output.setPartialName(this, "output_"+key.name)
//    }).asInstanceOf[T]
    getSignalAndInit(Outputs,key,default_init,"output")
  }

  def getOutputReg[T <: Data](key : Stageble[T]): T ={
    OutputRegs.getOrElse(key.asInstanceOf[Stageble[Data]],null).asInstanceOf[T]
  }

  def addPipelineOutputSignal[T<: Data](signals:(Stageble[T],Data)*): Unit ={
    for( (key,signal) <- signals ){
      Outputs += key.asInstanceOf[Stageble[Data]]->RegNext(signal)
    }
  }

  def >>(that:Stage)={
    NextStages += that
    that.LastStages += this
    that
    /*
    for ((key,signal) <- Outputs){
      that.input(key) := signal
      //that.Inputs += key->signal
    }
     */
  }

  def searchBackForSignal(key : Stageble[Data]): Data ={
    println(this.name + " search for "+ key.name)
    for(upstream <- LastStages){
      val output_reg=upstream.OutputRegs.getOrElse(key,null)
      if(output_reg!=null){
        println("find in output_reg")
        return output_reg
      }

      val input = upstream.Inputs.getOrElse(key,null)
      if(input!=null){
        println("find in input")
        val new_reg= RegNext(input).init(B(0))
        upstream.OutputRegs += key -> new_reg
        return new_reg
      }

      val uper_stream_signal=upstream.searchBackForSignal(key)
      if(uper_stream_signal != null){
        val new_reg=RegNext(uper_stream_signal).init(B(0))
        upstream.OutputRegs += key -> new_reg
        return new_reg
      }
    }
    null
  }

  def build()={
    println("start to build "+ this.name)
    for((key,signal)<-Outputs){
      OutputRegs += key->RegNext(signal).init(B(0))
    }
  }

  def inferConnections()={
    for((key,signal) <- Outputs){
      val input=Inputs.getOrElse(key,searchBackForSignal(key))
      if(input != null){
        signal := input
      }
    }
    for(stage <- NextStages){
      for( (key,signal) <- stage.Inputs){
        val reg=OutputRegs.getOrElse(key,stage.searchBackForSignal(key))
        if(reg != null){
          signal := reg
        }
      }
    }
  }
}
