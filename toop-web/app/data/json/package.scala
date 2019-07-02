package data

import io.circe.Encoder
import io.circe.generic.auto._
import models.{EvalFailure, EvalSuccess}

package object json {
  implicit val evalSuccessEncoder = Encoder[EvalSuccess]
  implicit val evalFailureEncoder = Encoder[EvalFailure]
}
