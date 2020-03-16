package server

import requests._
import responses._

import cats.effect._
import cats.implicits._

import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._

import org.http4s._
import org.http4s.circe._
import org.http4s.dsl.io._
import org.http4s.implicits._

import org.http4s.server.blaze._

import expressions.{Parser, Semantic}

object Main extends IOApp {
    val jsonApp = HttpRoutes.of[IO] {
        case req @ POST -> Root / "eval" => for {
            code <- req.as[Code]
            processed <- Parser.parse(code.code)
            .map(Semantic.eval _)
            .fold(
                err => BadRequest(ReduceError(err.toString).asJson),
                res => Ok(Reduced(res.toString).asJson),
            )
        } yield (processed)
    }.orNotFound

    def run(args: List[String]): IO[ExitCode] = {
        val port = args(0).toInt

        return BlazeServerBuilder[IO]
            .bindHttp(port, "0.0.0.0")
            .withHttpApp(jsonApp)
            .resource
            .use(_ => IO.never)
            .as(ExitCode.Success)
    }

}
