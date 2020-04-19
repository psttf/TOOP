package expressions

/**
 * Created by Александр on 05.03.2015.
 */
case class Number(n:Int) extends Term {
  override def toString = n.toString

  override def toFormat: String = if (n < 0) s"(0-${-n})" else n.toString

  lazy override val FV = Set[String]()
}
