package cli

import java.io.File

import scala.concurrent.{Future, TimeoutException}
import scala.io.Source
import scala.util.Failure
import expressions.{Parser, Semantic}

object Main {
  def main(args: Array[String]): Unit = {
    val parser = new scopt.OptionParser[Config]("sigmac") {
      head("sigmac", "0.1.1")

      arg[File]("<file>")
        .unbounded()
        .required()
        .validate(x =>
          if (x.exists) success
          else failure("File does not exist"))
        .validate(x => {
          val arr = x.getName.split('.')
          if (arr.length > 1)
            if (arr(arr.length - 1) == "sigma") success
            else failure("Not a valid file extension")
          else failure("Not a valid filename")
        })
        .action((x, c) => c.copy(files = c.files :+ x))
        .text("Sigma script")

      help("help").text("prints the usage text")
    }

    parser.parse(args, Config()) match {
      case Some(config) => {
        val content = Source.fromFile(config.files(0)).mkString
        val result = Parser.parse(content).map(Semantic.eval)
        result.fold(err => println(Console.RED + err + Console.RESET), res => println(res))
      }

      case None => println("No arguments given")
    }
  }
}
