package expressions

/**
 * Created by Александр on 02.03.2015.
 */
case class Variable(name:String) extends Term {
  override def toString = name

  override val FV = Set(this)
}
