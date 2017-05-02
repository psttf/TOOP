package expressions

/**
 * Created by Александр on 17.03.2015.
 */
case class Clone(a:Term) extends Term {
  lazy override val FV = a.FV
}
