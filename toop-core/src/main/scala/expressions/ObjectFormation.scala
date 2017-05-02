package expressions

/**
 * Created by Александр on 02.03.2015.
 */
case class ObjectFormation(methods:Map[String, Term]) extends Term {
  override def toString  =
      "["+ {
        for((l, s) <- methods.toList) yield l + " = " + s
      }.mkString(",") + "]"


  lazy override val FV = methods.foldLeft(Set[String]())((a,b) => a union b._2.FV)
}

