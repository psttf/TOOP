package expressions

/**
 * Created by Александр on 02.03.2015.
 */
case class Application(t1: Term, t2:Term) extends Term {
  override def toString = "("+t1+" "+t2+")"

  override val FV = t1.FV union t2.FV
}
