import scala.actors.Actor
import scala.actors.Actor._
import java.io.BufferedWriter
import java.io.FileWriter

case class msgWrapper(msg: Any, srcID: Int, srcTime: Int)

trait LogActor extends Actor {
  val myLogID = IDGenerator.generateID
  var myLogTime: Int = -1
  val myLogFileName = myLogID + ".log" //File Name Format "logid.log"
  writeToFile(myLogFileName, "(Log Format)CurrentActor@TimeMillis Order: Received/Sent [Message] from/to OtherActor\n", false) //Normal Output
  //writeToFile(myLogFileName, "digraph {\n", false) //For Graphviz
  //writeToFile(myLogFileName, "msc {\narcgradient = 10;\nActor" + myLogID + ";\n", false) //For MSCGen

  override def react(handler: PartialFunction[Any, Unit]) = {
    super.react {
      case msgWrapper(msg: Any, srcID: Int, srcTime: Int) =>
        myLogTime = (myLogTime max srcTime) + 1
        writeToFile(myLogFileName, "Actor-" + myLogID + "@" + System.currentTimeMillis() + " " + myLogTime + ": Received [" + msg + "] from Actor-" + srcID + "\n", true) //Normal Output
        //writeToFile(myLogFileName, "Actor" + srcID + " -> Actor" + myLogID + "[label=\"" + msg.toString.substring(0, msg.toString().length min 8) + myLogTime + "\"];" + "\n", true) //For Graphviz
        //writeToFile(myLogFileName, /*myLogTime + */ " Actor" + myLogID + " <= Actor" + srcID + "[label=\"" + msg.toString.substring(0, msg.toString().length min 8) + "\"];" + "\n", true) //For MSCGen
        handler.apply(msg)
    }
  }

  override def !(msg: Any): Unit = {
    val srcLogID = getSource(_.myLogID, 0)
    var srcLogTime = getSource(_.myLogTime, 0) + 1
    val srcLogFileName = getSource[String](_.myLogFileName, "Error.log")
    writeToFile(srcLogFileName, "Actor-" + srcLogID + "@" + System.currentTimeMillis() + " " + srcLogTime + ": Sent [" + msg + "] to Actor-" + myLogID + "\n", true) //Normal Output
    //writeToFile(srcLogFileName, "Actor" + srcLogID + " -> Actor" + myLogID + "[label=\"" + msg.toString.substring(0, msg.toString().length min 8) + srcLogTime + "\"];" + "\n", true) //For Graphviz
    //writeToFile(srcLogFileName, /*srcLogTime + */ " Actor" + srcLogID + " => Actor" + myLogID + "[label=\"" + msg.toString.substring(0, msg.toString().length min 8) + "\"];" + "\n", true) //For MSCGen
    super.!(msgWrapper(msg, srcLogID, srcLogTime))
  }

  def writeToFile(name: String, str: String, append: Boolean) {
    var bw: BufferedWriter = new BufferedWriter(new FileWriter(name, append))
    bw.write(str)
    bw.flush
    //bw.close()
  }

  def getSource[T](get: LogActor => T, default: T): T =
    self match {
      case x: LogActor => get(x)
      case _ => default
    }
}

object IDGenerator {
  var ID = -1
  def generateID: Int = {
    ID += 1
    ID
  }
}