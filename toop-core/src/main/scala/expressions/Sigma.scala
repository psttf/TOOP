package expressions

/**
 * Created by Александр on 02.03.2015.
 */
case class Sigma(override val variable:Variable, override val body:Term) extends Quantifier[Sigma](variable, body) {
  override def create(variable: Variable, body: Term): Sigma = Sigma(variable, body)

  override def toString = "(@"+variable+"=>"+body+")"

  //lazy override val FV = body.FV - variable.name
}
