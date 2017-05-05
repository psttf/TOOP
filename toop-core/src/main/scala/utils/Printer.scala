package utils

import expressions._

/**
  * Created by Qwex on 23.04.2017.
  */
object Printer {
  private val maxLength = 80

  def print(term : Term, indent : String, newStr : Boolean/*, length : Int*/) : String = term match {
    case ObjectFormation(methods) =>
      val printedMehods = methods.map({case (name, sigma) =>
        val prefix = s"$indent  $name = "
        val rec = print(sigma, "|" * prefix.length, false)
        s"$prefix$rec"
      })
      
      s"${if (newStr) indent else ""}[\n${printedMehods.mkString("\n")}\n$indent]"


    case MethodUpdate(obj:Term, label:String, method:Term) =>
      s"${print(obj, indent, false)}.${}<=${print(method, indent, false)}"

    case Lambda(v, t2) =>
       "("+print(indent, s"\\ $v => ", t2, false)+")"
       //val prefix = s"${if (newStr) indent else ""}\\ v => "
      //print(t2, indent)
    case Application(t1, t2) =>
     s"(${if(newStr) indent else ""}${print(t1, indent, false)} ${print(t2, indent, false)})"
     //print(indent, s"${print(t1, indent, false)} ", t2, false)

    case Sigma(v, t2) =>
      "("+print(indent, s"@ $v => ", t2, false)+")"

    case Variable(name) =>
      s"${if (newStr) indent else ""}$name"

    case MethodInvocation(obj, label) =>
      s"{${print(obj, indent, newStr)}.$label}"

    case _ => term.toString
  }

  private def print(indent : String, prefix : String, t : Term, newStr : Boolean) : String = {
    val prefixStr = s"${if (newStr) indent else ""}$prefix"
    s"$prefixStr${print(t, "|"* prefixStr.length, false)}"
  }
}
