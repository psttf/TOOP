package cli

import scopt.OptionParser
import java.io.File

object Main {
  def main(args: Array[String]): Unit = {
    val parser = new scopt.OptionParser[Config]("sigmac") {
      head("sigmac", "1.x")

      arg[File]("<file>").unbounded().text("optional unbounded args")
    }

    parser.parse(args, Config()) match {
      case Some(config) => println("asda")

      case None => println("aaaa")
    }
  }
}