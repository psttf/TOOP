package expressions

import scala.util.parsing.input.Positional

/**
 * Created by Александр on 02.03.2015.
 */
trait Term extends Positional {
  val FV : Set[String]
  def toFormat: String = toString
}
