import scala.io.Source


object Main {

  case class Elem(id:Int, label:String)

  case class MatchingProblem(set1:List[Elem], set2:List[Elem]) {

  }

  def main(args:Array[String]) = {
    println("Hello")

    Source.fromFile("./data/sm-bbt-in.txt").foreach {l =>
      print (l)
    }
  }

}