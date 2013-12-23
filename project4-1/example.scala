//imports
import scala.actors.Actor
import scala.actors.Actor._
import scala.math._
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scala.concurrent.duration._
import java.util.concurrent.TimeUnit
import scala.language.postfixOps
import java.io.FileOutputStream
import java.io.File
import java.io.FileInputStream
import java.io._
//import org.apache.commons.io._

//Messages
sealed trait GossipMessage
case object Go extends GossipMessage
case object Rumor extends GossipMessage
case class RemoveMe(me: LogActor) extends GossipMessage
case object Remind extends GossipMessage
case object Finish extends GossipMessage
case object ShutDown extends GossipMessage
case object NoNeighbor extends GossipMessage
case object PushRemind extends GossipMessage
case class Set(me: LogActor, allneighbor: ArrayBuffer[LogActor]) extends GossipMessage
case class Sets(s: Double) extends GossipMessage
case class SetTopo(topo: String) extends GossipMessage
case class RanAdd(ranN: LogActor) extends GossipMessage
case class Push(s: Double, w: Double) extends GossipMessage

//Object with main method
object example {
  def main(args: Array[String]) {
    val master = new Master(100, "full", "gossip")
    println("Building Network...")
    master.start()
    println("Protocol Start...")
  }
}

//Master Actor
class Master(numNodes: Int, topo: String, algo: String) extends LogActor {

  if (numNodes == 0) {
    exit()
  }
  val edge: Int = ceil(sqrt(numNodes)).toInt
  val num: Int = if (topo != "2D" && topo != "imp2D") numNodes else pow(edge, 2).toInt
  var numFin: Int = 0
  var time: Long = 0
  var allActors = new ArrayBuffer[LogActor]()
  var temp = new ArrayBuffer[LogActor]()
  var noNeiCount: Int = 0

  for (i <- 0 until num) {
    allActors += new GossipActor(topo) //Creat actors
    allActors(i).start //Start Actors
  }

  if (algo == "push-sum") {
    for (i <- 0 until num) {
      allActors(i) ! Sets(i.toDouble) //Set s
    }
  }

  def act() {
    topo match { //Set neighbor info
      case "full" =>
        for (i <- 0 until num) {
          allActors(i) ! Set(this, allActors - allActors(i)) //Set Neighbor Info
        }
      case "2D" =>

        allActors(0) ! Set(this, temp += (allActors(1), allActors(edge))) //set neighbor for first element in first line
        temp = new ArrayBuffer[LogActor]()

        for (i <- 1 to edge - 2) {
          allActors(i) ! Set(this, temp += (allActors(i - 1), allActors(i + 1), allActors(i + edge))) //first line except first and last
          temp = new ArrayBuffer[LogActor]()
        }

        allActors(edge - 1) ! Set(this, temp += (allActors(edge - 2), allActors(edge - 1 + edge))) //last one of first line
        temp = new ArrayBuffer[LogActor]()

        for (i: Int <- edge to num - edge - 1) { //Middle lines
          if (i % edge == 0) {
            allActors(i) ! Set(this, temp += (allActors(i - edge), allActors(i + edge), allActors(i + 1)))
            temp = new ArrayBuffer[LogActor]()
          } else if (i % edge == edge - 1) {
            allActors(i) ! Set(this, temp += (allActors(i - edge), allActors(i + edge), allActors(i - 1)))
            temp = new ArrayBuffer[LogActor]()
          } else {
            allActors(i) ! Set(this, temp += (allActors(i - edge), allActors(i + edge), allActors(i - 1), allActors(i + 1)))
            temp = new ArrayBuffer[LogActor]()
          }
        }

        allActors(num - edge) ! Set(this, temp += (allActors(num - edge - edge), allActors(num - edge + 1))) //set neighbor for first element in last line
        temp = new ArrayBuffer[LogActor]()

        for (i <- num - edge + 1 to num - 2) {
          allActors(i) ! Set(this, temp += (allActors(i - 1), allActors(i + 1), allActors(i - edge))) //last line except first and last element
          temp = new ArrayBuffer[LogActor]()
        }

        allActors(num - 1) ! Set(this, temp += (allActors(num - 2), allActors(num - 1 - edge))) //last one of last line
        temp = new ArrayBuffer[LogActor]()

      case "line" =>

        allActors(0) ! Set(this, temp += allActors(1)) //set neighbor for first element in line
        temp = new ArrayBuffer[LogActor]()

        for (i <- 1 to num - 2) {
          allActors(i) ! Set(this, temp += (allActors(i - 1), allActors(i + 1))) //line except first and last
          temp = new ArrayBuffer[LogActor]()
        }

        allActors(num - 1) ! Set(this, temp += allActors(num - 2)) //last one of line
        temp = new ArrayBuffer[LogActor]()

      case "imp2D" =>

        var ranList = allActors.clone()
        var ranN: LogActor = null
        allActors(0) ! Set(this, temp += (allActors(1), allActors(edge))) //set neighbor for first element in first line

        ranN = (ranList - allActors(0) -- temp)(Random.nextInt((ranList - allActors(0) -- temp).length))

        allActors(0) ! RanAdd(ranN)

        ranN ! RanAdd(allActors(0))

        ranList -= (allActors(0), ranN)

        temp = new ArrayBuffer[LogActor]()

        for (i <- 1 to edge - 2) {
          allActors(i) ! Set(this, temp += (allActors(i - 1), allActors(i + 1), allActors(i + edge))) //first line except first and last
          if (ranList.contains(allActors(i))) {
            ranN = (ranList - allActors(i) -- temp)(Random.nextInt((ranList - allActors(i) -- temp).length))
            allActors(i) ! RanAdd(ranN)
            ranN ! RanAdd(allActors(i))
            ranList -= (allActors(i), ranN)
          }
          temp = new ArrayBuffer[LogActor]()
        }

        allActors(edge - 1) ! Set(this, temp += (allActors(edge - 2), allActors(edge - 1 + edge))) //last one of first line
        if (ranList.contains(allActors(edge - 1))) {
          ranN = (ranList - allActors(edge - 1) -- temp)(Random.nextInt((ranList - allActors(edge - 1) -- temp).length))
          allActors(edge - 1) ! RanAdd(ranN)
          ranN ! RanAdd(allActors(edge - 1))
          ranList -= (allActors(edge - 1), ranN)
        }
        temp = new ArrayBuffer[LogActor]()

        for (i: Int <- edge to num - edge - 1) { //Middle lines
          if (i % edge == 0) {
            allActors(i) ! Set(this, temp += (allActors(i - edge), allActors(i + edge), allActors(i + 1)))
            if (ranList.contains(allActors(i)) && ranList.length >= 2) {
              ranN = (ranList - allActors(i) -- temp)(Random.nextInt((ranList - allActors(i) -- temp).length))
              allActors(i) ! RanAdd(ranN)
              ranN ! RanAdd(allActors(i))
              ranList -= (allActors(i), ranN)
            }
            temp = new ArrayBuffer[LogActor]()
          } else if (i % edge == edge - 1) {
            allActors(i) ! Set(this, temp += (allActors(i - edge), allActors(i + edge), allActors(i - 1)))
            if (ranList.contains(allActors(i)) && ranList.length >= 2) {
              ranN = (ranList - allActors(i) -- temp)(Random.nextInt((ranList - allActors(i) -- temp).length))
              allActors(i) ! RanAdd(ranN)
              ranN ! RanAdd(allActors(i))
              ranList -= (allActors(i), ranN)
            }
            temp = new ArrayBuffer[LogActor]()
          } else {
            allActors(i) ! Set(this, temp += (allActors(i - edge), allActors(i + edge), allActors(i - 1), allActors(i + 1)))
            if (ranList.contains(allActors(i)) && ranList.length >= 2) {
              ranN = (ranList - allActors(i) -- temp)(Random.nextInt((ranList - allActors(i) -- temp).length))
              allActors(i) ! RanAdd(ranN)
              ranN ! RanAdd(allActors(i))
              ranList -= (allActors(i), ranN)
            }
            temp = new ArrayBuffer[LogActor]()
          }
        }

        allActors(num - edge) ! Set(this, temp += (allActors(num - edge - edge), allActors(num - edge + 1))) //set neighbor for first element in last line
        if (ranList.contains(allActors(num - edge)) && ranList.length >= 2) {
          ranN = (ranList - allActors(num - edge) -- temp)(Random.nextInt((ranList - allActors(num - edge) -- temp).length))
          allActors(num - edge) ! RanAdd(ranN)
          ranN ! RanAdd(allActors(num - edge))
          ranList -= (allActors(num - edge), ranN)
        }
        temp = new ArrayBuffer[LogActor]()

        for (i <- num - edge + 1 to num - 2) {
          allActors(i) ! Set(this, temp += (allActors(i - 1), allActors(i + 1), allActors(i - edge))) //last line except first and last element
          if (ranList.contains(allActors(i)) && ranList.length >= 2) {
            ranN = (ranList - allActors(i) -- temp)(Random.nextInt((ranList - allActors(i) -- temp).length))
            allActors(i) ! RanAdd(ranN)
            ranN ! RanAdd(allActors(i))
            ranList -= (allActors(i), ranN)
          }
          temp = new ArrayBuffer[LogActor]()
        }

        allActors(num - 1) ! Set(this, temp += (allActors(num - 2), allActors(num - 1 - edge))) //last one of last line
        if (ranList.contains(allActors(num - 1)) && ranList.length >= 2) {
          ranN = (ranList - allActors(num - 1) -- temp)(Random.nextInt((ranList - allActors(num - 1) -- temp).length))
          allActors(num - 1) ! RanAdd(ranN)
          ranN ! RanAdd(allActors(num - 1))
          ranList -= (allActors(num - 1), ranN)
        }
        temp = new ArrayBuffer[LogActor]()

    }

    self ! Go //Start from self-----------------------------------------------

    loop {
      react {
        case Go =>
          time = System.currentTimeMillis()
          if (algo == "gossip") {
            allActors(Random.nextInt(num)) ! Rumor
          } else if (algo == "push-sum") {
            allActors(Random.nextInt(num)) ! Push(0, 0)
          } else {
            println("No such algorithm supported!\nTry \"gossip\" or \"push-sum\"!")
            allExit()
          }

        case Finish =>
          numFin += 1
          if (numFin == num) {
            println("Protocol Finished.")
            println("Number of Nodes: " + num)
            println("Time: " + (System.currentTimeMillis() - time) + " milliseconds")
            allExit()
            //println("Converged Nodes: " + (num - noNeiCount) + "\nNot Converged Nodes: " + noNeiCount)
            //println("Converge Ratio: " + ((num - noNeiCount).toDouble * 100 / num.toDouble) + "%")
          }
          if (algo == "push-sum" && numFin == 1) {
            println("Protocol Finished.")
            println("Number of Nodes: " + num)
            //println("Converged Nodes: " + (num - noNeiCount) + "\nNot Converged Nodes: " + noNeiCount)
            //println("Converge Ratio: " + ((num - noNeiCount).toDouble * 100 / num.toDouble) + "%")
            println("Time: " + (System.currentTimeMillis() - time) + " milliseconds")
            allExit()
          }
        case NoNeighbor =>
          noNeiCount += 1
      }
    }
  }

  def allExit() = {
    var a: LogActor = null
    for (a <- allActors) {
      a ! ShutDown
    }
    exit()
  }

}
//GossipActor Actor
class GossipActor(topo: String) extends LogActor {
  //import context._

  var neighbor = new ArrayBuffer[LogActor]()
  var rumorCount: Int = 0
  var boss: LogActor = null
  var isDone: Boolean = false
  var mys: Double = 1
  var myw: Double = 1
  var lastvalue: Double = 0
  var currvalue: Double = 0
  var valueCount: Int = 0
  var finished: Boolean = false
  var reached: Boolean = false
  def act() {
    loop {
      react {

        case Push(s, w) =>
          if (!isDone) {
            mys = (s + mys) / 2
            myw = (w + myw) / 2
            currvalue = mys / myw
            if (abs(currvalue - lastvalue) <= 1e-3 && w != 0) {
              valueCount += 1
            } else {
              valueCount = 0
            }
            lastvalue = currvalue
            if (valueCount < 30 && neighbor.length > 0) {
              //context.system.scheduler.scheduleOnce(0.1 milliseconds, neighbor(Random.nextInt(neighbor.length)), Push(mys, myw))
              neighbor(Random.nextInt(neighbor.length)) ! Push(mys, myw)
              //          if (!reached) {
              //            self ! PushRemind
              //            reached = true
              //          }
            } else {
              isDone = true
              for (ac: LogActor <- neighbor)
                ac ! RemoveMe(this)
              if (!finished) {
                boss ! Finish
                finished = true
              }
              //context.stop(self)
            }

          }
        case PushRemind =>
          mys /= 2
          myw /= 2
          println("Stuck")
        //neighbor(Random.nextInt(neighbor.length)) ! Push(mys, myw)
        //context.system.scheduler.scheduleOnce(1000 milliseconds, neighbor(Random.nextInt(neighbor.length)), Push(mys, myw))
        //self ! Push(0, 0)
        //context.system.scheduler.scheduleOnce(1000 milliseconds, self, PushRemind)

        case Rumor =>
          if (!isDone) {
            if (!reached) {
              if (!finished) {
                boss ! Finish
                finished = true
              }
              reached = true
              self ! Remind
            }
            rumorCount += 1
            if (rumorCount < 10 && neighbor.length > 0) {
              //neighbor(Random.nextInt(neighbor.length)) ! Rumor ????????????????
              //if(!firstRemind){
              //neighbor(Random.nextInt(neighbor.length)) ! Rumor
              //firstRemind=true
              //}
            } else {
              //sender ! Rumor
              //neighbor(Random.nextInt(neighbor.length)) ! Rumor
              //println(self+":"+neighbor.length)
              isDone = true
              for (ac: LogActor <- neighbor)
                ac ! RemoveMe(this)
              //neighbor.clear
              //boss ! Finish
              //context.stop(self)
            }
          }
        //      else {
        //        //weird += 1
        //        //println(weird)
        //        sender ! Remind
        //      }
        case Remind =>
          if (rumorCount < 10 && neighbor.length > 0 && !isDone) {
            neighbor(Random.nextInt(neighbor.length)) ! Rumor
            //println("Wait for long")
            topo match {
              case "line" =>
                //self.wait(10)
                self ! Remind
              //context.system.scheduler.scheduleOnce(800 milliseconds, self, Remind)
              case "2D" =>
                //self.wait(10)
                self ! Remind
              //context.system.scheduler.scheduleOnce(800 milliseconds, self, Remind)
              case _ =>
                //self.wait(10)
                self ! Remind
              //context.system.scheduler.scheduleOnce(800 milliseconds, self, Remind)
            }
            //context.system.scheduler.scheduleOnce(50 milliseconds, self, Remind)
            //Scheduler.scheduleOnece()//system.//scheduler().scheduleOnce(Duration.create(50, TimeUnit.MILLISECONDS), testActor, "foo", system.dispatcher(), null);
            //var old = System.currentTimeMillis()
            //while (System.currentTimeMillis() - old <= 10) {
            //do nothing
            //}
            //neighbor(Random.nextInt(neighbor.length)) ! Rumor
            //self ! Remind
          }

        case RemoveMe(me) =>
          if (!isDone) {
            neighbor -= me
            if (neighbor.length <= 0) {
              isDone = true
              if (!finished) {
                boss ! NoNeighbor
                boss ! Finish
                finished = true
              }
              //context.stop(self)
            }
          }

        case Set(me, allneighbor) =>
          neighbor ++= allneighbor
          boss = me

        case Sets(s) =>
          mys = s
          lastvalue = mys / myw

        case RanAdd(ranN) =>
          neighbor += ranN

        case ShutDown =>
          exit()
      }
    }
  }
}