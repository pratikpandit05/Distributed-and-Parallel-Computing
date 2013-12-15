import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.{ Props, Deploy, Address, AddressFromURIString }
import scala.math
import java.lang.Boolean
import java.util.ArrayList
import scala.collection.GenSeq
import scala.util.Random
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

case class FirstMsg(i:Int, j:Int)
case class Result(x: Long)


class Worker(i: Int, j: Int, m:Int, n:Int, worker1:Array[Array[akka.actor.ActorRef]] ) extends Actor {
	var neighbours= new ArrayList[akka.actor.ActorRef]
			var (s_actor: Double, w_actor: Double) = ( n*i+j+1.0,1.0)
			var (s_last: Double, w_last: Double) = (0.0,1.0)
			
			var count:Int = 0
			def valid(x:Integer, y:Integer): Boolean ={
					if((x<0) || (x>m-1) || (y<0) || (y>n-1) ){ return false}
					return true
			}	
			
			var timer:akka.actor.Cancellable = null 
			
			def receive = {
			case "grid" =>
	
				if( valid(i-1,j)==true){neighbours.add(worker1(i-1)(j))}
				if( valid(i+1,j)==true){neighbours.add(worker1(i+1)(j))}
				if( valid(i,j-1)==true){neighbours.add(worker1(i)(j-1))}
				if( valid(i,j+1)==true){neighbours.add(worker1(i)(j+1))}
				//println(neighbours)
			
			case "imp2D" =>
				//println ("i ="+i+" j="+j+"\n")
				if( valid(i-1,j)==true){neighbours.add(worker1(i-1)(j))}
				if( valid(i+1,j)==true){neighbours.add(worker1(i+1)(j))}
				if( valid(i,j-1)==true){neighbours.add(worker1(i)(j-1))}
				if( valid(i,j+1)==true){neighbours.add(worker1(i)(j+1))}
				
				//val rand = new Random(System.currentTimeMillis());
				val rand_x = Random.nextInt(m);
				val rand_y = Random.nextInt(n);
				// Check if the random node is not same as node
				if(rand_x !=i || rand_y != j){neighbours.add(worker1(rand_x)(rand_y))}
				//println(neighbours+"\ni ="+i+" j="+j+"\n")
			
			case "full" =>
			  	for (x<- 0 to m-1) {
						for (y <- 0 to n-1 ) {
							if(x!=i && y!=j )neighbours.add(worker1(x)(y))
						}
				}
			case "line" =>
			  	if( valid(i,j-1)==true){neighbours.add(worker1(i)(j-1))}
				if( valid(i,j+1)==true){neighbours.add(worker1(i)(j+1))}
				
			// Implementation of the protocols 	
			case "gossip" =>
				if (count == 0) {
				  context.parent ! FirstMsg(i,j)
				  timer = context.system.scheduler.schedule(0 milliseconds,50 milliseconds,self,"reminder")
				}
				count+=1 
				if (count > 10){ timer.cancel() }
				
			case "reminder" =>
			  val random_index = Random.nextInt(neighbours.size());
				neighbours.get(random_index) ! "gossip"
				
				
				
			 //Push-sum protocol 
			case (s:Double,w:Double) => 
			    
				s_actor+=s
				w_actor+=w  
				var ratio = Math.abs((s_actor/w_actor)-(s_last/w_last))
				//if( ratio < 0.0000001){
				//	println("Ratio = "+ratio+ "Index = ("+i+"," +j+")")
				//}
				
				if(Math.abs((s_actor/w_actor)-(s_last/w_last))< 0.0000000001){
					count = count +1
				}else {count = 0}
				
				if (count < 3){
					s_actor = s_actor/2
					w_actor = w_actor/2
	
					s_last = s_actor
					w_last = w_actor
					
					//val rand = new Random(System.currentTimeMillis());
					//println("neighbours.size = "+ neighbours.size())
					val random_index = Random.nextInt(neighbours.size());
					//println("random_index = "+random_index)					
					neighbours.get(random_index) ! (s_actor, w_actor)
				}else {	
					println("Actor terminated _"+i+"_"+j)
					// Notify manager for the termination message, 
					//which inturn will print the total executiontime
					context.parent ! "terminate"
				}
			}
}

class Manager extends Actor {
	var doneActorsCount = 0
	var startTime:Long = 0
	var gossipMsg:Int = 0
	var actors_num:Int = 0
			
			var (i:Int, j:Int) = (0, 0)

			def receive = {
			case (numActors: Integer, topo:String, proto:String ) =>
			  actors_num = numActors
			  var temp = (Math.ceil(Math.sqrt(numActors * 1.0))).toInt

			  
			  
			  if(topo!="line"){
			    var worker = Array.ofDim[akka.actor.ActorRef](temp, temp)
				for (i<- 0 to temp-1) {
					for (j <- 0 to temp-1 ) {
						worker(i)(j) = context.actorOf(Props(new Worker(i,j, temp, temp, worker)), name = "WorkerActor_" + i+"_"+j)
					}
				}
				if(topo =="2D" ){
					for (i<- 0 to temp-1) {
						for (j <- 0 to temp-1 ) {
							worker(i)(j) ! "grid"
						}
					}
				}else if (topo =="imp2D"){
				     for (i<- 0 to temp-1) {
						for (j <- 0 to temp-1 ) {
							worker(i)(j) ! "imp2D"
						}
					}
				 }else if (topo =="full"){
				     for (i<- 0 to temp-1) {
						for (j <- 0 to temp-1 ) {
							worker(i)(j) ! "full"
						}
				     }
				 }
				if( proto =="push-sum"){
					worker(temp/2)(temp/2) ! (0.0,0.0)	  	
			    }else {
			    	worker(0)(0) ! "gossip"
			    }
				startTime = System.currentTimeMillis()
				
			  }else {
			    print("In line" )
			    var worker = Array.ofDim[akka.actor.ActorRef](1, numActors)
			    for (i <- 0 to numActors-1 ) {
						worker(0)(i) = context.actorOf(Props(new Worker(0, i, 1, numActors, worker)), name = "WorkerActor_" + i+"_"+0)
				}
			    for (j <- 0 to numActors - 1 ) {
							worker(0)(j) ! "line"
				}
			    if ( proto == "push-sum"){
			    	worker(0)(0) ! (0.0,0.0)
			    }else {
			    	worker(0)(0) ! "gossip"
			    }
			    // Fire the timer right-after first msg is sent
			    startTime = System.currentTimeMillis()
			  }
			println("Manager Started")
			
		
			// Manager keeps track of the msgs receieved and terminates when all 
			// actors get the gossip atleast once, Prints the total Execution time
			case FirstMsg(i:Int, j:Int)=>
			  gossipMsg+=1 
			  if( gossipMsg == actors_num ){
			    println("Gossip Algoritm running time "+(System.currentTimeMillis()- startTime))
			    context.system.shutdown()
			  }
			
			 // Push Sum is terminated, and total execution time is printed
			case "terminate" =>
			  println("Total convergence time = "+( System.currentTimeMillis()- startTime))
			  context.system.shutdown()
	}
}

object Main extends App {
	//try {
		val system = ActorSystem("HelloSystem")
				val manager = system.actorOf(Props[Manager], name = "ManagerActor")
				manager ! (args(0).toInt, args(1).toString(), args(2).toString())
				//manager ! "start"
	//} catch {
	//case e: Exception => println("Exception in Main object")
	//}
}
