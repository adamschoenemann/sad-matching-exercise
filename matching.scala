import scala.io.Source
import scala.collection.mutable.Queue
import scala.collection.mutable.Stack
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

object Matching {

  case class Proposer(id:Int, label:String, var prefs:Stack[Int]) {

    var engaged = 0
    def isUnengaged = (engaged == 0)
  }

  // prefs is converted from textual format to an array indexed by (id/2)
  // to preferability
  case class Responder(id:Int, label:String, var prefs:ArrayBuffer[Int]) {

    def isUnengaged = (engaged == 0)
    var engaged = 0

    def lookupPref(id:Int) = {
      prefs(id/2)
    }

    def accepts(p:Proposer):Boolean = (lookupPref(p.id) > lookupPref(this.engaged))
  }


  case class Solution(proposers:ArrayBuffer[Proposer], responders:ArrayBuffer[Responder]) {
    override def toString:String = {
      proposers.foldLeft ("") ((str, prop) => {
        str + prop.label + " -- " + responders(prop.engaged/2 - 1).label + "\n"
      })
    }
  }

  case class MatchingProblem(proposers:ArrayBuffer[Proposer], responders:ArrayBuffer[Responder]) {

    def getProposer(id:Int):Proposer = proposers(id/2)
    def getResponder(id:Int):Responder = responders(id/2 - 1) // alternatively ((id-1)/2)

    // mutates MatchingProblem and returns a new solution
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
          // println(r.label + " accepts " + p.label + " over " + old.label)

          r.engaged = p.id
          p.engaged = r.id
        } else { // he does not find a woman
          propQueue.enqueue(p)
        }
      }
      Solution(proposers, responders)
    }
  }

  def parseProblem(input:String) = {

    def isPersonLine(line:String):Boolean =
        line.trim.length > 0 && !isPreferenceLine(line)

    def isPreferenceLine(line:String):Boolean = line.indexOf(":") > -1

    def stackToArrayBuffer[A](s:Stack[A], buf:ArrayBuffer[A]):ArrayBuffer[A] = {
      s.foreach (x => {
        buf += x
      })
      buf
    }

    def newBuffer[A](size:Int, default:A):ArrayBuffer[A] = {
      var buf = new ArrayBuffer[A](size)
      for (_ <- 0 until size)
        buf += default
      buf
    }

    var n = 0
    var proposers:ArrayBuffer[Proposer] = null
    var responders:ArrayBuffer[Responder]  = null



    Source.fromFile(input).getLines().foreach {l =>
      if (l.startsWith("#")) { // its a comment
        // do nothing
      } else if (l.trim.length == 0) { // empty line
        // do nothing
      } else if (l.startsWith("n=")) { // specifies n
        n = l.split("=")(1).toInt
        proposers  = newBuffer[Proposer](n, null)
        responders = newBuffer[Responder](n, null)
      } else if (isPreferenceLine(l)) {
          val Array(idStr, prefsStr) = l.split(":")
          val id = idStr.replace(":", "").toInt
          val prefsArr = prefsStr.trim.split(" ").map(_.toInt)
          if (id % 2 == 0) { // is a responder
            val r = responders(id/2 - 1)
            val prefs = newBuffer[Int](n, 0)
            var n2 = n
            for (prf <- prefsArr) {
              prefs(prf/2) = n2
              n2 -= 1
            }
            r.prefs = prefs
          } else { // is a proposer
            val p = proposers(id/2)
            val prefs = Stack[Int]()
            for (prf <- prefsArr) {
              prefs.push(prf)
            }
            p.prefs = prefs.reverse
          }
      } else if (isPersonLine(l)) {
        val Array(idStr, labelStr) = l.split(" ")
        val id = idStr.toInt
        if (id % 2 == 0) {
          responders(id/2 - 1) = Responder(id, labelStr, ArrayBuffer(n))
        } else {
          proposers(id/2) = Proposer(id, labelStr, Stack())
        }
      } else {
        throw new RuntimeException("unexpected input line: " + l)
      }
    }

    val problem = MatchingProblem(
        proposers,
        responders
      )
    // println(problem)

    val solution = problem.solve()
    solution

  }

}