package expressions

/**
 * Created by Александр on 02.03.2015.
 */
case class Sigma(override val variable:Variable, override val body:Term) extends Quantifier[Sigma](variable, body) {
  override def create(variable: Variable, body: Term): Sigma = Sigma(variable, body)

  override def toString = "(@"+variable+"=>"+body+")"

  override def toFormat: String = s"@${variable.toFormat} => ${body.toFormat}"

  //lazy override val FV = body.FV - variable.name
}
