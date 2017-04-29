package expressions

/**
 * Created by Александр on 05.03.2015.
 */
case class Number(n:Int) extends Term {
  override def toString = n.toString

  override val FV = Set[Variable]()
}
