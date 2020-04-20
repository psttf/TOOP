package requests

import cats.effect._
import io.circe.generic.auto._

import org.http4s.circe._

final case class Code(code: String)
object Code { implicit val decoder = jsonOf[IO, Code] }
