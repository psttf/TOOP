package expressions

trait Arithmetic extends Term {
  def t1 : Term
  def t2 : Term
  lazy val FV1 = Semantic.FV(t1) union Semantic.FV(t2)
  override val FV = t1.FV union t2.FV
}

case class Add(t1 : Term, t2 : Term) extends Arithmetic
case class Subtract(t1 : Term, t2 : Term) extends Arithmetic
