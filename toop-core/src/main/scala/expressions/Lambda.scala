package expressions

/**
 * Created by Александр on 02.03.2015.
 */
case class Lambda(override val variable:Variable, override val body:Term) extends Quantifier[Lambda](variable, body) {
  override def create(variable: Variable, body: Term): Lambda = Lambda(variable, body)

  override def toString = "(\\"+variable+"=>"+body+")"

  override def toFormat: String = s"(\\${variable.toFormat} => ${body.toFormat})"

  //lazy override val FV = body.FV - variable.name
}
