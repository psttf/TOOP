package server

import requests._
import responses._

import cats.effect._
import cats.implicits._

import io.circe.generic.auto._
import io.circe.syntax._

import org.http4s._
import org.http4s.circe._
import org.http4s.dsl.io._
import org.http4s.implicits._

import org.http4s.server.blaze._

import expressions.{Parser, Semantic, SemanticState}

import org.http4s.server.middleware._


object Main extends IOApp {
    val jsonApp = HttpRoutes.of[IO] {
        case req @ POST -> Root / "eval" => for {
            code <- req.as[Code]
            processed <- Parser.parse(code.code)
            .map(Semantic.eval(_) match {
                case SemanticState(term, history) => term match {
                    case Right(t) => Ok(Reduced(
                        t.toString,
                        history.map(_.toString),
                    ).asJson)
                    case Left(err) => BadRequest(ReduceError(
                        err.toString,
                        history.map(_.toString)
                    ).asJson)
                }
            }).fold(
                err => BadRequest(ReduceError(err.toString, List()).asJson),
                res => res
            )
        } yield processed
    }.orNotFound

    val corsConfig = CORSConfig(
        anyOrigin = true,
        anyMethod = true,
        allowCredentials = true,
        maxAge = 1 * 24 * 60 * 60,
    )

    def run(args: List[String]): IO[ExitCode] = {
        val port = sys.env("PORT").toInt
        val service = CORS(jsonApp, corsConfig)

        BlazeServerBuilder[IO]
            .bindHttp(port, "0.0.0.0")
            .withHttpApp(service)
            .resource
            .use(_ => IO.never)
            .as(ExitCode.Success)
    }

}
