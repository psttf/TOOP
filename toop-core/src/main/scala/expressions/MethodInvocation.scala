package expressions

/**
 * Created by Александр on 02.03.2015.
 */
case class MethodInvocation(obj:Term, label:String) extends Term {
  override def toString = "{"+obj+"."+label+"}"

  lazy override val FV = obj.FV
}
