import scala.actors._
import java.io._
import java.util.Date

class actorlog extends Actor {
  
  def act() {}
  
  def SenderLogger(AcInst: Actor, DestinationAcInst: Actor, CaseSend: String) {
      log(AcInst, "Message sent", "Message", "send", DestinationAcInst, CaseSend)
   }
   
   def ReceiveLogger(AcInst: Actor, SourceAcInst: Actor, CaseReceived: String) {
     log(AcInst, "Message received", "Message", "receive", SourceAcInst, CaseReceived)
   }
  
    def WarningLogger(AcInst: Actor, Warning: String) {
     log(AcInst, Warning, "Warning", null, null, null)
   }   

   def InfoLogger(AcInst: Actor, Information: String) {
     log(AcInst, Information, "Info", null, null, null)
   }
   
   def ErrorLogger(AcInst: Actor, Error: String) {
     log(AcInst, Error, "Error", null, null, null)
   }
   
   
  def log(AcInst: Actor, Information: String, T_Log: String, SendOrReceiveFlag: String, OtherAcInst: Actor, Message: String) {
    var ActorID: String = AcInst.hashCode().toString()
    var ActorState: String = AcInst.getState.toString()
    var F_Name: String = "Actor_" + ActorID + ".log"
    
    var D_Ins = new Date()
    val FIns: File = new File(F_Name)
    var FilIns: FileWriter = null

    var F_Exist = 0
    
    if (!FIns.exists())
      FilIns = new FileWriter(FIns.getName())
    else
    {
      FilIns = new FileWriter(FIns.getName(), true)
      F_Exist = 1
  	}
    
    var writer = new PrintWriter(FilIns)
    
    if(F_Exist == 0)
    writer.write("Time\t\t\t\t\t\t\tType\t\tSource\t\t\tDestination\t\t\tInformation\t\t\t\tState\tCase")
    
    if(OtherAcInst == null)    
    	writer.write("\n"+ D_Ins.toLocaleString()+"\t\t"+ T_Log + "\t\t" + "N/A"+ "\t\t\t\t" + "N/A"+ "\t\t\t\t\t" +Information + "\t\t" + ActorState)
    else if(SendOrReceiveFlag == "send")
    	writer.write("\n"+ D_Ins.toLocaleString()+"\t\t"+ T_Log + "\t\t"  + AcInst.hashCode() + "\t\t" + OtherAcInst.hashCode() + "\t\t\t"  + Information + "\t\t\t" + ActorState + "\t\t"+ Message)
    else if(SendOrReceiveFlag == "receive")
    	writer.write("\n"+ D_Ins.toLocaleString()+"\t\t"+ T_Log + "\t\t" + OtherAcInst.hashCode() + "\t\t" + AcInst.hashCode() + "\t\t\t" + Information + "\t\t" + ActorState + "\t\t" + Message)	
    	
    writer.close()
  }

}