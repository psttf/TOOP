package expressions

trait Arithmetic extends Term {
  def t1 : Term
  def t2 : Term
  lazy val FV = Semantic.FV(t1) union Semantic.FV(t2)
}

case class Add(t1 : Term, t2 : Term) extends Arithmetic
case class Subtract(t1 : Term, t2 : Term) extends Arithmetic
