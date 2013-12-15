import scala.actors.Actor
import scala.actors.Actor._

abstract class CopQuestions
case object Start extends CopQuestions
case object AskQuestion extends CopQuestions
case object Analyse extends CopQuestions

abstract class AccusedAnswers
case object Answer extends AccusedAnswers
case object Stop extends AccusedAnswers

class Cop(count: Int, accused: Actor) extends actorlog {
  
  override
  def act() {
    
    var CopIns = new Cop(count, accused)
    InfoLogger(CopIns,"Interr arranged")
    
    var NoOfQuestions = count
    
    loop {
      react {
        case Start =>
          InfoLogger(CopIns,"Interr begins")
          SenderLogger(CopIns,accused,Answer.toString())
          accused ! Answer
          NoOfQuestions = NoOfQuestions - 1
          
        case AskQuestion =>
          ReceiveLogger(CopIns,self,Analyse.toString())
          SenderLogger(CopIns,accused,Answer.toString())
          accused ! Answer
          NoOfQuestions = NoOfQuestions - 1
          
        case Analyse =>
          if (NoOfQuestions % 100 == 0)
          {
            InfoLogger(CopIns,"Answer given")
          }
          
          if (NoOfQuestions > 0)
          {
            ReceiveLogger(CopIns,accused,Answer.toString())
            SenderLogger(CopIns,self,AskQuestion.toString())
            self ! AskQuestion
          }
          else {
            InfoLogger(CopIns,"Questions finished")
            ReceiveLogger(CopIns,accused,Answer.toString())
            SenderLogger(CopIns,accused,Stop.toString())
            accused ! Stop
            exit('stop)
          }
      }
    }
  }
}

class Accused extends actorlog { 
   
  override
  def act() {
    
    var AccusedIns = new Accused()    
    var NoOfAnswers = 0
    
    loop {
      react {
        case Answer =>
          if (NoOfAnswers % 100 == 0)
          {
            InfoLogger(AccusedIns,"Question Asked")
          }
          ReceiveLogger(AccusedIns,sender.asInstanceOf[Actor],AskQuestion.toString())
          SenderLogger(AccusedIns,sender.asInstanceOf[Actor],Analyse.toString())
          sender ! Analyse
          NoOfAnswers = NoOfAnswers + 1
          
        case Stop =>
          ReceiveLogger(AccusedIns,sender.asInstanceOf[Actor],Analyse.toString())
          InfoLogger(AccusedIns,"Interr finished")
          exit('stop)
      }
    }
  }
}

object example extends App {
  val accused = new Accused
  val cop = new Cop(1000, accused)
  cop.start
  accused.start
  cop ! Start
}