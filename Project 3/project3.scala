
import scala.math
import scala.util.Random
import akka.actor.Actor
import akka.actor.Props
import akka.actor._
import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.Props
import scala.reflect._
import java.io.UnsupportedEncodingException
import java.security.MessageDigest
import java.security.NoSuchAlgorithmException
import java.util.Iterator
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import java.math.BigInteger
import scala.Array._
import akka.actor.ActorRef


case class InPeer(fcid:String, refCon:ActorRef);
case class PInitializer;
case class Route(h_count:Int, destNId:String, destNRef:ActorRef);
case class NewLeaf(nLId:String, nLRef:ActorRef);
case class UpdatePeer(lkeys:Array[String], lvalues:Array[ActorRef], tkeys:Array[Array[String]], tvalues:Array[Array[ActorRef]])
case class TPrint;
case class InfoNode(nodeId:String, nodeRef:ActorRef);
case class FullRouter(h_count:Int);
case class UpdateTable;
case class BeginSend(PAll:Array[ActorRef], rSId:Array[String]);
case class NewSender;

object project3 {

  def main(args: Array[String]): Unit = {
    if(args.length == 0 || args.length != 2){
      println("Improper arguements")
    }else if(args.length == 2){
	    val base = 8;
	    val length = 8;
	    val numNodes = args(0).toInt;
	    var numRequests = args(1).toInt;
	    var RStop = 200;
	    
	    var ran_id:Array[Int] = new Array[Int](numNodes);
	    var rSId:Array[String] = new Array[String](numNodes);
	    
	    val system = ActorSystem("PastryNodeSystem")
	    var statusKeeper:ActorRef = system.actorOf(Props(new StatusKeeper(numNodes, numRequests)))
	    var PAll:Array[ActorRef] = new Array[ActorRef](numNodes);
	    
	    ran_id = getRandom(numNodes);
	    for(i <- 0 until numNodes){
	      rSId(i) = sha1Code(ran_id(i), base, length);
	      var r_NId:Int = 0;
	      if(i == 0){
	        PAll(i) = system.actorOf(Props(new Peer(statusKeeper, "", null, rSId(i), base, length)));
	      
	      }else{
	        r_NId = NoOneRandom(i, i);
	        PAll(i) = system.actorOf(Props(new Peer(statusKeeper, rSId(r_NId), PAll(r_NId), rSId(i), base, length)));
	    
	      }
	    }
	    
	   
	    var r_NId = NoOneRandom(numNodes, 0);
	    PAll(0) ! InPeer(rSId(r_NId), PAll(r_NId));
	    
	    Thread.sleep(1000);
	    for(i <- 1 until numNodes){
	      PAll(i) ! PInitializer;
	    }
	    
	    Thread.sleep(numNodes * 200);
	    
	    for(i <- 0 until numRequests){
	      var x = Random.nextInt(numNodes);
	      var y = NoOneRandom(numNodes, x)
	      PAll(x) ! Route(0, rSId(y), PAll(y));
	      Thread.sleep(RStop);
	    }	    
	    Thread.sleep((numRequests * RStop) + 5000);
	    system.shutdown;
	    
	}
  }
  
  def NoOneRandom(maxNum:Int, nti:Int): Int = {
    var r_num:Int = 0;
    do{
      r_num = Random.nextInt(maxNum);
    } while (r_num == nti);
    return r_num;
  }
    
  def sha1Code(r_num:Int, base:Int, length:Int): String = {
    var rCode:String = "";
    val md = java.security.MessageDigest.getInstance("SHA-1");
    val hash:Array[Byte] = md.digest(r_num.toString.getBytes);
    rCode = BigInt(1, hash).toString(base);
    rCode = rCode.substring(0, length)
	return rCode;
  }
  
  def getRandom(n:Int): Array[Int] = {
    var Set:HashSet[Int] = new HashSet[Int];
    do{
      var r_num = Random.nextInt(100000000);
      Set.add(r_num);
    } while (Set.size != n);
    
    var randomInts:Array[Int]=new Array[Int](n);
    Set.copyToArray(randomInts);
    return randomInts;
  }
}



class Peer(var statusKeeper:ActorRef, var fcid:String, var refCon:ActorRef, var selfId:String, var base:Int, var length:Int) 
extends Actor {
  var lkeys:Array[String] = new Array[String](2*base);
  var lvalues:Array[ActorRef] = new Array[ActorRef](2*base);
  var tkeys = ofDim[String](length, base)
  var tvalues = ofDim[ActorRef](length, base)
  var UpdateTableCount:Int = 0;
  
  var finalDest = "";
  
  override def preStart() = {
    superUpdater(selfId, self);
    if(null != refCon){
      superUpdater(fcid, refCon);
    }
  }
  
  override def postStop() = {
  }
  
  def receive = {
    case InPeer(firstContId:String, firstContRef:ActorRef) => {
      fcid = firstContId;
      refCon = firstContRef;
      superUpdater(fcid, refCon);
      Thread.sleep(500);
      refCon ! NewLeaf(selfId, self);
    }
    
    case PInitializer => {
      Thread.sleep(500);
      if(null == refCon){
      }
      refCon ! NewLeaf(selfId, self);
    }
    
    case NewLeaf(nLId:String, nLRef:ActorRef) => {
      if(!nLId.equals(selfId)){
        var nextHop:InfoNode = getClosestNode(nLId);
        var l_mod:Int = superUpdater(nLId, nLRef);
	    if(l_mod == 1){
	      nextHop.nodeRef ! NewLeaf(nLId, nLRef);
	      nLRef ! UpdatePeer(lkeys, lvalues, tkeys, tvalues);
	    }
      }
    }
    
    case Route(h_count:Int, destNId:String, destNRef:ActorRef) => {
      if(destNId.equals(selfId)){
        statusKeeper ! FullRouter(h_count);
      }else{
        var nextHop:InfoNode = getClosestNode(destNId);
        Thread.sleep(500);
        
        if(finalDest == nextHop.nodeId){
          nextHop = NoOneRLeaf(finalDest);
          if(nextHop != null){
          }else{  
          }
          finalDest = "";
        }else{
          finalDest = nextHop.nodeId;
        }
        
        if(nextHop==null || selfId == nextHop.nodeId){
 
        }else{
          nextHop.nodeRef ! Route(h_count+1, destNId, destNRef);
        }
    
        var isUpdated:Int = superUpdater(destNId, destNRef);
        if(isUpdated == 1){
          for(i <- 0 until lvalues.size){
	        if(lvalues(i) != null && lkeys(i)!=selfId){
	          lvalues(i) ! UpdatePeer(lkeys, lvalues, tkeys, tvalues);
	          statusKeeper ! UpdateTable;
	        }
	      }
        }
       
      }
    }
    
    case UpdatePeer(newlkeys:Array[String], newlvalues:Array[ActorRef], newtkeys:Array[Array[String]], newtvalues:Array[Array[ActorRef]]) => {
      var addedKeys:List[String] = Nil;
      var listUpdated = 0;
      
      for(i <- 0 until newlkeys.size){
        if(null!=newlkeys(i) && !lkeys.contains(newlkeys(i))){
          addedKeys::=newlkeys(i);
          listUpdated += superUpdater(newlkeys(i), newlvalues(i));
        }
      }
      
      for(i <- 0 until newtkeys.size){
        for(j <- 0 until newtkeys(i).size){
          if(null!=newtkeys(i)(j) && !addedKeys.contains(newtkeys(i)(j))){
            addedKeys::=newtkeys(i)(j);
            listUpdated += superUpdater(newtkeys(i)(j), newtvalues(i)(j));
          }
        }
      }
      
      UpdateTableCount += listUpdated;
      
      if(UpdateTableCount>0){
        UpdateTableCount = 0;
        for(i <- 0 until lvalues.size){
          if(lvalues(i) != null && lkeys(i)!=selfId){
            lvalues(i) ! UpdatePeer(lkeys, lvalues, tkeys, tvalues);
            statusKeeper ! UpdateTable;
          }
        }
        
      }
    }
    
    case TPrint => {
      superPrinter();
    }
    
    case _ => {
    }
    
  }
  
  def superPrinter(){
    var s:String = "";
  
    s = s + "\nmyId: "+selfId+" Leaf Table:\n";
    for(i <- 0 until lkeys.size){

      s = s+lkeys(i)+" ";
    }
  
    s = s+"\nmyId: "+selfId+" Routing Table:\n";
    for(i <- 0 until tkeys.size){
      for(j <- 0 until tkeys(i).size){
   
        if(tkeys(i)(j) != null) {
          s = s + tkeys(i)(j)+"\t";
        }else{
          s = s + "- null -"+"\t";
        }
      }
      s = s + "\n";

    }
    
    println(s);
  }
  
  def superUpdater(newNodeId:String, newNodeRef:ActorRef):Int = {
    var anyChange:Int = 0;
    var nodeId:String = newNodeId;
    var nodeRef:ActorRef = newNodeRef;
    
    var brk:Int = 0;
    if(!lkeys.contains(newNodeId)){
    	for(i <- 0 until lkeys.size){
	      if(brk!=1){
	   
	    	if(lkeys(i) != null){
	    
	    	  var closerId = getCloserNode(selfId, nodeId, lkeys(i));
	    	  if(closerId == 1){
	    	    var tempId = lkeys(i);
	    	    var tempRef = lvalues(i);
	    	    
	    	    lkeys(i) = nodeId;
	    	    lvalues(i) = nodeRef;
	    	    
	    	    nodeId = tempId;
	    	    nodeRef = tempRef;
	    	    
	    	    anyChange = 1;
	    	  }
	    	}else {
	    	  lkeys(i) = nodeId;
	    	  lvalues(i) = nodeRef;
	    	  brk = 1;
	    	}
	      }
	    }
    }
    
    
    nodeId = newNodeId;
    nodeRef = newNodeRef;
   
    var n_row:Int = 0;
    var n_col:Int = 0;
 
    
    for(i <- 0 until selfId.length()){
      if(selfId.substring(0, i).equals(nodeId.substring(0, i))){
        n_row = i;
        n_col = getRTcol(nodeId, n_row);
        if(tkeys(n_row)(n_col) == null){
	      tkeys(n_row)(n_col) = nodeId;
	      tvalues(n_row)(n_col) = nodeRef;
	      anyChange = 1;
	    }else{
	      var closerId = getCloserNode(selfId, nodeId, tkeys(n_row)(n_col));
		  if(closerId == 1){
		    tkeys(n_row)(n_col) = nodeId;
		    tvalues(n_row)(n_col) = nodeRef;
		  }
	    }
      }
    }
    
    return anyChange;
  }
  
  def getRTrow(subjectNodeId:String, nodeId:String):Int = {
    var n_row:Int = -1;
    for(i <- 0 until subjectNodeId.length()){
      if(subjectNodeId.substring(0, i).equals(nodeId.substring(0, i))){
        n_row = i;
      }
    }
    return n_row;
  }
  
  def getRTcol(nodeId:String, n_row:Int):Int = {
    var n_col:Int = 0;
    n_col = Integer.parseInt(""+nodeId.charAt(n_row), base);
    return n_col;
  }
  
  def getClosestNode(nodeId:String):InfoNode = {
    var result:InfoNode = null;
    var leaf:InfoNode = null;
    var routing:InfoNode = null;
    leaf = getLSNode(nodeId);
    if(leaf == null || leaf.nodeId == null){
      leaf = getLSNearest(nodeId);
      if(null!=leaf){
        result = leaf;
      }
      try{
        routing = getRTNearest(nodeId);
        var closer = getCloserNode(nodeId, leaf.nodeId, routing.nodeId);
        if(closer <= 1){
          result = leaf;
        }else if(closer == 2){
          result = routing;
        }
      }catch {
        case e: Exception => {
          superPrinter();
        }
      }
    }else{
      result = leaf;
    }
    return result;
  }
  
  def getLSNode(nodeId:String):InfoNode = {
    var brk:Int = 0;
    var result:InfoNode = null;
    for(itr:Int <- 0 until lkeys.size){
      if(lkeys(itr)!=null && nodeId.equals(lkeys(itr))){
        result = InfoNode(lkeys(itr), lvalues(itr));
      }
    }
    return result;
  }
  
  def getLSNearest(nodeId:String):InfoNode = {
      var result:InfoNode = null; 
	  for(i <- 0 until lkeys.size){
	    if(lkeys(i)!=null && lkeys(i)!=selfId){
	      if(result==null){
	        result = InfoNode(lkeys(i), lvalues(i));
	      }else{
	        if(getCloserNode(nodeId, result.nodeId, lkeys(i)) == 2){
	          result = InfoNode(lkeys(i), lvalues(i));
	        }
	      }
	    }
	  }
    return result;
  }
  
  def getRTNearest(nodeId:String):InfoNode = {
    var result:InfoNode = null;
    var n_row:Int = 0;
    var n_col:Int = 0;
    
    for(i <- 0 until selfId.length()){
      if(selfId.substring(0, i).equals(nodeId.substring(0, i))){
        n_row = i;
        n_col = getRTcol(nodeId, n_row);
        if(tvalues(n_row)(n_col)!=null && tkeys(n_row)(n_col)!=selfId){
          result = InfoNode(tkeys(n_row)(n_col), tvalues(n_row)(n_col));
        }
      }
    }
    if(null==result){
      for(n_rowI <- 0 until tkeys.size){
        for(n_colJ <- 0 until tkeys(n_rowI).size){
          if(tkeys(n_rowI)(n_colJ) != null && tkeys(n_rowI)(n_colJ) != selfId){
            if(result==null){
              result = InfoNode(tkeys(n_rowI)(n_colJ), tvalues(n_rowI)(n_colJ));
            }else{
              if(getCloserNode(nodeId, result.nodeId, tkeys(n_rowI)(n_colJ)) == 2){
                result = InfoNode(tkeys(n_rowI)(n_colJ), tvalues(n_rowI)(n_colJ));
              }
            }
          }
        }
      }
    }
    
    return result;
  }
  
  def getCloserNode(nodeID:String, nodeID1:String, nodeID2:String):Int = {
      var dd:BigInt = 0;
      var dd1:BigInt = 0;
      var dd2:BigInt = 0;
      var max:BigInt = 0;
      
  
      var mx:BigInt = 1;
      var l = nodeID.length();
      for(i <- 0 until nodeID.length()){
    
        
        dd += mx * Integer.parseInt(nodeID.substring(l-i-1, l-i), base);
        dd1 += mx * Integer.parseInt(nodeID1.substring(l-i-1, l-i), base);
        dd2 += mx * Integer.parseInt(nodeID2.substring(l-i-1, l-i), base);
        max += mx * (base-1);
        mx = mx * base;
      }
 
      
      var difference1 = dd-dd1;
      if(difference1<0) difference1 = -1*difference1;
      if(difference1>(max/2)) difference1 = max-difference1;
      
      var difference2 = dd-dd2;
  
      if(difference2<0) difference2 = -1*difference2;
      if(difference2>(max/2)) difference2 = max-difference2;
  
      
      if(difference1==difference2){
        return 0;
      }else if(difference1<difference2){
        return 1;
      }else if(difference1>difference2){
        return 2;
      }
      return 0;
  }
  
  def NoOneRLeaf(nti:String): InfoNode = {
    var result:InfoNode = null;
    var FilledL = 0;
    var maxNum:Int = lkeys.size;
    
    for(i <- 0 until maxNum){
      if(lkeys(i)!=null){
        FilledL += 1;
      }
    }
    var r_num:Int = -1;
    if(FilledL>2){
      do{
        r_num = Random.nextInt(maxNum);
	    if(lkeys(r_num) == null || lkeys(r_num) == nti || lkeys(r_num) == selfId){
	        
	    }else{
	      result = InfoNode(lkeys(r_num), lvalues(r_num));
	    }   
	  }while(result == null);
	}
    return result;
  }
}


class StatusKeeper(var numNodes:Int, var numRequests:Int) extends Actor{
  var UTCount = 0;
  var maxHops:Int = 10;
  var routesComplete = 0;
  var hopsStat:Array[Int] = new Array[Int](maxHops+1);
  
  var PAll:Array[ActorRef] = new Array[ActorRef](numNodes);
  var rSId:Array[String] = new Array[String](numNodes);
  
  override def postStop() = {
    printStatus();
  }
  
  def printStatus(){
    var weightedSum:Double = 0;
    var hopsCount:Double = 0;
    for(i <- 1 until hopsStat.size){
      hopsCount += hopsStat(i);
      weightedSum += i * hopsStat(i);
    }
    var avgHops:Double = weightedSum/hopsCount;
    println("Average number of hops traversed are "+avgHops);
  }

  
  def receive={
    case FullRouter(h_count:Int) => {
      if(h_count<=maxHops){
        hopsStat(h_count) += 1;
      }else{
        hopsStat(0) += 1;
      }
 
    }
    
    case UpdateTable => {
      UTCount += 1;
    }
    
    case BeginSend(peers:Array[ActorRef], shI:Array[String]) => {
      PAll = peers;
      rSId = shI;
      self ! NewSender;
    }
    
    case NewSender => {
      routesComplete += 1;
      var i = Random.nextInt(numNodes);
      var j = NoOneRandom(PAll.size, i)
      PAll(i) ! Route(0, rSId(j), PAll(j));
  
    }
    
    case _ =>{
   
    }
    
  }
  
  def NoOneRandom(maxNum:Int, nti:Int): Int = {
    var r_num:Int = 0;
    do{
      r_num = Random.nextInt(maxNum);
    } while (r_num == nti);
    return r_num;
  }
  
}

