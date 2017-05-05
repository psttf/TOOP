package expressions

/**
 * Created by Александр on 02.03.2015.
 */
abstract class Quantifier[T <: Term](val variable:Variable, val body:Term) extends Term
{
  def create(variable: Variable = this.variable, body:Term = this.body):T

  lazy override val FV = body.FV - variable.name
}
