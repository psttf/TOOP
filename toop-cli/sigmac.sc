#!/usr/bin/env amm

import ammonite.ops._
import scala.io.Source

@main
def main(s: String) = {
  if (!(s contains ".")) exit("Not a valid fileName")
  val fileName :: extension :: nil = s.split('.').toList
  if (extension != "sigma") exit("Not a valid file extension")
  println(s"Script compiling for ${s}")
  ammonite.Main(
    predefCode = "println(\"Starting Debugging!\")"
  ).run(
  )
  for (line <- Source.fromFile(s).getLines) {
    println(line)
  }

}