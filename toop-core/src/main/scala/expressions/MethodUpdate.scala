package expressions

/**
 * Created by Александр on 02.03.2015.
 */
case class MethodUpdate(obj:Term, label:String, method:Term) extends Term
{
  override def toString = "("+obj+"."+label+"<="+method+")"

  override def toFormat = s"(${obj.toFormat}.$label <= ${method.toFormat})"

  lazy override val FV = obj.FV union method.FV
}
