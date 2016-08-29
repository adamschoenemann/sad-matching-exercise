
import scala.io.Source
import scala.collection.immutable.Queue
import scala.collection.immutable.Stack
import scala.collection.immutable.IndexedSeq
import scala.collection.immutable.HashMap
import scala.collection.mutable.ArrayBuffer

object Main {

  trait Elem {
    val id:Int
    val label:String
  }

  case class Proposer(id:Int, label:String, prefs:Stack[Int], engaged:Int) extends Elem {

    def isUnengaged = (engaged == 0)

    def unengage:Proposer = copy(engaged = 0)
  }

  // prefs is converted from textual format to an array indexed by (id/2)
  // to preferability
  case class Responder(id:Int, label:String, prefs:IndexedSeq[Int], engaged:Int) extends Elem {

    def isUnengaged = (engaged == 0)

    def lookupPref(id:Int) = {
      prefs(id/2)
    }

    def accepts(p:Proposer):Boolean = (lookupPref(p.id) > this.engaged)
  }


  case class Solution(proposers:IndexedSeq[Proposer], responders:IndexedSeq[Responder]) {
    def getResponder(id:Int) = responders(id/2 - 1)
    def getProposer(id:Int)  = proposers(id/2)


    override def toString:String = {
      proposers.foldLeft ("") ((str, prop) => {
        str + prop.label + " -- " + responders(prop.engaged/2 - 1).label + "\n"
      })
    }

    def addPair(p:Proposer, r:Responder):Solution = {
      val proposers_ = proposers.updated(p.id/2, p.copy(engaged = r.id))
      val responders_ = responders.updated(r.id/2 - 1, r.copy(engaged = p.id))
      copy(proposers = proposers_, responders = responders_)
    }

    def unengage(p:Proposer):Solution = {
      val proposers_ =
        proposers.map(q => if (q.id == p.id) q.unengage else q)
      copy(proposers = proposers_)
    }
  }

  case class MatchingProblem(length:Int, proposers:IndexedSeq[Proposer], responders:IndexedSeq[Responder]) {

    def getProposer(id:Int):Proposer = proposers(id/2)
    def getResponder(id:Int):Responder = responders(id/2 - 1) // alternatively ((id-1)/2)

    def solve():Solution = {

      def loop(unengaged:Queue[Proposer], sol:Solution):Solution = {
        if (unengaged.isEmpty) {
          sol
        } else {
          val (proposer, unengaged_) = unengaged.dequeue
          val (rid, prefs2) = proposer.prefs.pop2
          val p = proposer.copy(prefs = prefs2)
          val r = sol.getResponder(rid)
          if (r.isUnengaged) {
            loop(unengaged_, sol.addPair(p, r))
          } else if (r.accepts(p)) {
            // dump the previous
            val old = sol.getProposer(r.engaged)
            val old_ = old.unengage

            val unengaged__ = unengaged_.enqueue(old)
            loop(unengaged__, sol.unengage(old_).addPair(p, r))
          } else {
            println("no success for " + p)
            loop(unengaged_.enqueue(p), sol)
          }
        }

      }
      val init = Solution(proposers, responders)
      val queue = Queue(proposers: _*)
      loop(queue, init)
    }
  //     var propQueue = Queue[Proposer]()
  //     for (p <- proposers) {
  //       propQueue.enqueue(p)
  //     }

  //     while(!propQueue.isEmpty) {
  //       var p = propQueue.dequeue()
  //       var r = getResponder(p.prefs.pop())

  //       if (r.isUnengaged) {
  //         r.engaged = p.id
  //         p.engaged = r.id
  //       } else if (r.accepts(p)) {
  //         // dump the previous
  //         val old = getProposer(r.engaged)
  //         old.engaged = 0
  //         propQueue.enqueue(old)

  //         r.engaged = p.id
  //         p.engaged = r.id
  //       } else { // he does not find a woman
  //         propQueue.enqueue(p)
  //       }
  //     }
  //     Solution(proposers, responders)
  //   }
  }

  def main(args:Array[String]) = {

    def isPersonLine(line:String):Boolean =
        line.trim.length > 0 && !isPreferenceLine(line)

    def isPreferenceLine(line:String):Boolean = line.indexOf(":") > -1

    type ParserProblem = (Int, Array[Proposer], Array[Responder])

    val problem = Source.fromFile("./data/sm-bbt-in.txt")
      .getLines().foldLeft[Option[ParserProblem]] (None) ((acc, l:String) => {
        if (l.startsWith("#")) { // its a comment
          println(l)
          // do nothing
          acc
        } else if (l.trim.length == 0) { // empty line
          println("line")
          // do nothing
          acc
        } else if (l.startsWith("n=")) { // specifies n
          val n = l.split("=")(1).toInt
          println("n found: " + n)
          Some((
            n, new Array[Proposer](n), new Array[Responder](n))
          )
        } else if (isPreferenceLine(l)) {
          val (n, proposers, responders) = acc.get
          val Array(idStr, prefsStr) = l.split(":")
          val id = idStr.replace(":", "").toInt
          val prefsArr = prefsStr.trim.split(" ").map(_.toInt)
          if (id % 2 == 0) { // is a responder
            val r = responders(id/2 - 1)
            val (_,prefs) = prefsArr.foldRight (n, new Array[Int](n)) {(prf, acc) =>
              val (n2, prefs2) = acc
              prefs2(prf/2) = n2
              (n2-1, prefs2)
            }
            responders(id/2 - 1) = r.copy(prefs = prefs.toIndexedSeq)
            acc
          } else { // is a proposer
            val p = proposers(id/2)
            val prefs = prefsArr.foldLeft (Stack[Int]()) (_.push(_))
            // for (prf <- prefsArr) {
            //   prefs.push(prf)
            // }
            proposers(id/2) = p.copy(prefs = prefs)
            acc
          }
        } else if (isPersonLine(l)) {
          val (n, proposers, responders) = acc.get
          val Array(idStr, labelStr) = l.split(" ")
          val id = idStr.toInt
          if (id % 2 == 0) {
            responders(id/2 - 1) = Responder(id, labelStr, IndexedSeq.empty, 0)
          } else {
            proposers(id/2) = Proposer(id, labelStr, Stack(), 0)
          }
          acc
        } else {
          throw new RuntimeException("unexpected input line: " + l)
        }
      })

    val (n, proposers, responders) = problem.get
    val mproblem = MatchingProblem(
      n,
      proposers.toIndexedSeq,
      responders.toIndexedSeq
    )

    val solution = mproblem.solve()
    println(solution)


    // def stackToArray[A](s:Stack[A], buf:ArrayBuffer[A]):ArrayBuffer[A] = {
    //   s.foreach (x => {
    //     buf += x
    //   })a
    //   buf
    // }

    // def newBuffer[A](size:Int, default:A):ArrayBuffer[A] = {
    //   var buf = new ArrayBuffer[A](size)
    //   for (_ <- 0 until size)
    //     buf += default
    //   buf
    // }

    // var n = 0
    // var proposers:ArrayBuffer[Proposer] = null
    // var responders:ArrayBuffer[Responder]  = null



    // Source.fromFile("./data/sm-bbt-in.txt").getLines().foreach {l =>
    //   if (l.startsWith("#")) { // its a comment
    //     // do nothing
    //   } else if (l.trim.length == 0) { // empty line
    //     // do nothing
    //   } else if (l.startsWith("n=")) { // specifies n
    //     n = l.split("=")(1).toInt
    //     proposers  = newBuffer[Proposer](n, null)
    //     responders = newBuffer[Responder](n, null)
    //   } else if (isPreferenceLine(l)) {
    //       val Array(idStr, prefsStr) = l.split(":")
    //       val id = idStr.replace(":", "").toInt
    //       val prefsArr = prefsStr.trim.split(" ").map(_.toInt)
    //       if (id % 2 == 0) { // is a responder
    //         val r = responders(id/2 - 1)
    //         val prefs = newBuffer[Int](n, 0)
    //         var n2 = n
    //         for (prf <- prefsArr) {
    //           prefs(prf/2) = n2
    //           n2 -= 1
    //         }
    //         r.prefs = prefs
    //       } else { // is a proposer
    //         val p = proposers(id/2)
    //         val prefs = Stack[Int]()
    //         for (prf <- prefsArr) {
    //           prefs.push(prf)
    //         }
    //         p.prefs = prefs
    //       }
    //       val person =
    //         if (id % 2 == 0) responders(id/2 - 1) else proposers(id/2)

    //   } else if (isPersonLine(l)) {
    //     val Array(idStr, labelStr) = l.split(" ")
    //     val id = idStr.toInt
    //     if (id % 2 == 0) {
    //       responders(id/2 - 1) = Responder(id, labelStr, Array(n))
    //     } else {
    //       proposers(id/2) = Proposer(id, labelStr, Stack())
    //     }
    //   } else {
    //     throw new RuntimeException("unexpected input line: " + l)
    //   }
    // }



  }

}