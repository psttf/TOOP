package expressions

/**
 * Created by Александр on 02.03.2015.
 */
case class MethodInvocation(obj:Term, label:String) extends Term {
  override def toString = "{"+obj+"."+label+"}"

  override val FV = obj.FV
}
