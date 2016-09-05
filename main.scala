
import java.io._
import scala.io.Source

object Main {

  def main(args:Array[String]):Unit = {
    solveProblem(args)
  }

  def solveProblem(args:Array[String]):Unit = {
    val output = args.headOption.map(in => {
      try {
        val cwd = new File(".").getAbsolutePath()
        println("cwd: " + cwd)
        val solution =
          Matching.parseProblem("./data/" + in)
        solution.toString
      } catch {
        case ex:FileNotFoundException => println(ex.getMessage)
      }
    }).getOrElse("An input string must be provided")

    println(output)
  }

  def test():Unit = {

    val inputFiles = new File("./data").listFiles.filter(_.getName().contains("in.txt"))

    for (f <- inputFiles) {
      val solution = Matching.parseProblem(f.getAbsolutePath())
      val outFileName  = f.getAbsolutePath().replace("in.txt", "out.txt")
      try {
        val outFileCont  = Source.fromFile(outFileName).getLines.mkString("\n")
        // println("SOLUTION:")
        // println(solution.toString)
        // println("\n VS \n")
        // println(outFileCont)
        assert(solution.toString.trim == outFileCont.trim)
        println(f.getName().replace("-in.txt", "") + " is verified")
      } catch {
        case ex:FileNotFoundException => println(ex.getMessage)
      }


    }
  }
}