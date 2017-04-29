package expressions

/**
 * Created by Александр on 02.03.2015.
 */
case class ObjectFormation(methods:Map[String, Term]) extends Term {
  var name = "nameless"
  var code = counter.i

  counter.i = counter.i+1

  override def toString  =
    //s"[$name]"
      "\n::"+name+" "+code+" "+FV+"::\n["+ {
        for((l, s) <- methods.toList) yield l + " = " + s
      }.mkString(",") + "]"


  override val FV = methods.foldLeft(Set[Variable]())((a,b) => a union b._2.FV)
}

object counter {
  var i = 0
}
