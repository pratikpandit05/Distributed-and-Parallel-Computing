
import scala.math._
import akka.actor._
import scala.language.postfixOps

class Worker extends Actor {
  def receive= {
	 case (x1: Long, y: Long, l: Long, k: Long, manager: ActorRef, prob: Int) =>
	   for (i <- y to (y+l-1)) {
          val xi = x1 + (i-1)*(i+k)*k
          val root = round(sqrt(xi))
          if(xi == root*root)
            manager ! (i) 
	   }
	  manager ! (123l,prob) 
    }
}

class Manager extends Actor {
   
   var count: Int = 0
    
   def receive = {
         
     	case (n: Long, k: Long, np: Int)  =>
     	  val actorSystem = ActorSystem("MySystem")
     	  val x1 = k*(k+1)*(2*k+1)/6
     	  val prob = np  
     	  val worker = actorSystem.actorOf(Props[Worker])
     	  for (j <- 1 to prob) {  
     		  worker ! (x1, 1+(j-1)*n/prob ,n/prob, k, self, prob)
    }
     	  
     
		case (first: Long) =>  
		  println(first)
        
		
		case (marker: Long, prob: Int) =>   
		  count += 1
		  if (count >= prob)
             sys.exit()
  }
}

object project1 extends App{
    val n = if (args.length > 0) args(0) toLong else 10000000l  
    val k = if (args.length > 1) args(1) toLong else 2l       
    val np = 10        
    

    val actorSystem = ActorSystem("MySystem")
    val manager = actorSystem.actorOf(Props[Manager])
    manager ! (n, k, np)
}
