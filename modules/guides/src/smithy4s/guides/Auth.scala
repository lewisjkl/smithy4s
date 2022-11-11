/*
 *  Copyright 2021-2022 Disney Streaming
 *
 *  Licensed under the Tomorrow Open Source Technology License, Version 1.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *     https://disneystreaming.github.io/TOST-1.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package smithy4s.guides

import smithy4s.guides.auth._
import cats.effect._
import cats.implicits._
import org.http4s.implicits._
import org.http4s.ember.server._
import org.http4s._
import com.comcast.ip4s._
import smithy4s.http4s.SimpleRestJsonBuilder
import org.http4s.server.Middleware
import org.typelevel.ci.CIString
import scala.concurrent.duration.Duration
import smithy4s.kinds.{FunctorAlgebra, PolyFunction5, Kind1}
import smithy4s.Hints
import org.http4s.headers.Authorization
import cats.data.OptionT

final case class APIKey(value: String)
type APIKeyIOLocal = IOLocal[Option[APIKey]]
object APIKeyIOLocal {
  def empty: IO[APIKeyIOLocal] = IOLocal(Option.empty[APIKey])
}

object HelloWorldAuthImpl extends HelloWorldAuthService[IO] {
  def sayWorld(): IO[World] = World().pure[IO]
}

object AuthChecker {
  def isAuthorized(token: APIKey): IO[Boolean] = {
    IO.pure(
      token.value.nonEmpty
    ) // put your logic here, currently just makes sure the token is not empty
  }
}

object AuthExampleRoutes {
  import org.http4s.server.middleware._

  private def helloRoutes(
      local: APIKeyIOLocal
  ): Resource[IO, HttpRoutes[IO]] =
    SimpleRestJsonBuilder
      .routes(
        new AuthedService(HelloWorldAuthImpl, local)
          .build(isTokenValid = AuthChecker.isAuthorized)
      )
      .resource

  def all(local: APIKeyIOLocal): Resource[IO, HttpRoutes[IO]] =
    helloRoutes(local)
}

// A transformation on the smithy4s service that will return
// an unauthorized error if the IOLocal does not contain a currently
// valid token. It is possible to implement this with different auth on
// different endpoints, but this example keeps it as one auth type per
// service for simplicity.
final class AuthedService[Alg[_[_, _, _, _, _]]](
    original: FunctorAlgebra[Alg, IO],
    authTokenLocal: APIKeyIOLocal
)(implicit
    serviceProvider: smithy4s.Service.Provider[Alg]
) {
  private val service = serviceProvider.service

  private def checkAuth(isTokenValid: APIKey => IO[Boolean]): IO[Unit] =
    authTokenLocal.get.flatMap {
      case Some(token) =>
        isTokenValid(token).flatMap {
          case true  => IO.unit // authorized, do nothing
          case false =>
            // token not valid
            IO.raiseError(new NotAuthorizedError("Not authorized!"))
        }
      case None =>
        // no token found
        IO.raiseError(new NotAuthorizedError("Not authorized!"))
    }

  def build(
      isTokenValid: APIKey => IO[Boolean]
  ): FunctorAlgebra[Alg, IO] = {
    val transfo = new PolyFunction5[service.Operation, Kind1[IO]#toKind5] {
      def apply[I, E, O, SI, SO](
          op: service.Operation[I, E, O, SI, SO]
      ): IO[O] = {
        val (input, ep) = service.endpoint(op)
        val process = service.toPolyFunction(original)(ep.wrap(input))
        val check: IO[Unit] = {
          service.hints.get[smithy.api.HttpBearerAuth] match {
            case Some(_) => checkAuth(isTokenValid)
            case None =>
              IO.unit // there is no @httpBearerAuth trait on the service
          }
        }
        check *> process
      }
    }
    service.fromPolyFunction[Kind1[IO]#toKind5](transfo)
  }
}

// IF `Authorization` header is present with `Bearer` format,
// The value of said header will be placed in an IOLocal
// so it can be accessed by the AuthedService implementation
// above.
object AuthMiddleware {
  def http4sMiddleware(
      local: APIKeyIOLocal 
    ): HttpRoutes[IO] => HttpRoutes[IO] = { routes =>
    HttpRoutes[IO] { request =>
      val maybeAuth = request.headers
        .get[`Authorization`]
        .collect {
          case Authorization(Credentials.Token(AuthScheme.Bearer, value)) =>
            value
        }
        .map { APIKey.apply }
      OptionT.liftF(local.set(maybeAuth)) *> routes(request)
    }
  }
}

// test with `curl localhost:9000/hello -H 'Authorization: Bearer Some'`
object AuthExampleMain extends IOApp.Simple {
  val run = (for {
    local <- Resource.eval(APIKeyIOLocal.empty)
    routes <- AuthExampleRoutes.all(local)
    server <- EmberServerBuilder
      .default[IO]
      .withPort(port"9000")
      .withHost(host"localhost")
      .withHttpApp(AuthMiddleware.http4sMiddleware(local)(routes).orNotFound)
      .build
  } yield server).useForever
}
