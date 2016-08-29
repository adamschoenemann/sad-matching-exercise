import scala.io.Source
import scala.collection.mutable.Queue
import scala.collection.mutable.Stack
import scala.collection.mutable.ArrayBuffer

object Main {

  trait Elem {
    val id:Int
    val label:String
  }

  case class Proposer(id:Int, label:String, prefs:Stack[Int]) extends Elem {

    var engaged = 0
    def isUnengaged = (engaged == 0)
  }

  // prefs is converted from textual format to an array indexed by (id/2)
  // to preferability
  case class Responder(id:Int, label:String, prefs:ArrayBuffer[Int]) extends Elem {

    var isUnengaged = (engaged == 0)
    var engaged = 0

    def lookupPref(id:Int) = {
      prefs(id/2)
    }

    def accepts(p:Proposer):Boolean = (lookupPref(p.id) > this.engaged)
  }


  case class Solution(proposers:Array[Proposer], responders:Array[Responder])

  case class MatchingProblem(proposers:ArrayBuffer[Proposer], responders:ArrayBuffer[Responder]) {

    def getProposer(id:Int):Proposer = proposers(id/2)
    def getResponder(id:Int):Responder = responders(id/2 - 1) // alternatively ((id-1)/2)

    def solve():Solution = {
      var propQueue = Queue[Proposer]()
      for (p <- proposers) {
        propQueue.enqueue(p)
      }

      while(!propQueue.isEmpty) {
        var p = propQueue.dequeue()
        var r = getResponder(p.prefs.pop())

        if (r.isUnengaged) {
          r.engaged = p.id
          p.engaged = r.id
        } else if (r.accepts(p)) {
          // dump the previous
          val old = getProposer(r.engaged)
          old.engaged = 0
          propQueue.enqueue(old)

          r.engaged = p.id
          p.engaged = r.id
        } else { // he does not find a woman
          propQueue.enqueue(p)
        }
      }
      sys.error("undefined")
      // Solution(proposers, responders)
    }
  }

  def main(args:Array[String]) = {

    def isPersonLine(line:String):Boolean =
        line.trim.length > 0 && !isPreferenceLine(line)

    def isPreferenceLine(line:String):Boolean = line.indexOf(":") > -1

    def stackToArrayBuffer[A](s:Stack[A], buf:ArrayBuffer[A]):ArrayBuffer[A] = {
      var i = 0
      s.foreach (x => {
        buf(i) = x
        i += 1
      })
      buf
    }

    var n = 0

    var proposers = Stack[Proposer]()
    var responders = Stack[Responder]()

    Source.fromFile("./data/sm-bbt-in.txt").getLines().foreach {l =>
      if (l.startsWith("#")) { // its a comment
        // do nothing
      } else if (l.trim.length == 0) { // empty line
        // do nothing
      } else if (l.startsWith("n=")) { // specifies n
        n = l.split("=")(1).toInt
      } else if (isPreferenceLine(l)) {

      } else if (isPersonLine(l)) {
        val Array(idStr, labelStr) = l.split(" ")
        val id = idStr.toInt
        if (id % 2 == 0) {
          responders.push(Responder(id, labelStr, ArrayBuffer(n)))
        } else {
          proposers.push(Proposer(id, labelStr, Stack()))
        }
      } else {
        throw new RuntimeException("unexpected input line: " + l)
      }
    }

    val problem = MatchingProblem(
        stackToArrayBuffer[Proposer](proposers, new ArrayBuffer[Proposer](n)),
        stackToArrayBuffer[Responder](responders, new ArrayBuffer[Responder](n))
      )

    println(problem)

  }

}