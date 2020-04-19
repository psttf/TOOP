package expressions

/**
 * Created by Александр on 02.03.2015.
 */
trait Term {
  val FV : Set[String]
  def toFormat: String = toString
}
