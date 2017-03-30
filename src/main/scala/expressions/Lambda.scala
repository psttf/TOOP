package expressions

/**
 * Created by Александр on 02.03.2015.
 */
case class Lambda(override val variable:Variable, override val body:Term) extends Quantifier[Lambda](variable, body) {
  override def create(variable: Variable, body: Term): Lambda = Lambda(variable, body)

  override def toString = "(\\"+variable+"=>"+body+")"
}
