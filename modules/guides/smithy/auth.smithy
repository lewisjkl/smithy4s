$version: "2"

namespace smithy4s.guides.auth

use alloy#simpleRestJson

@simpleRestJson
@httpBearerAuth // add this here
service HelloWorldAuthService {
  version: "1.0.0",
  operations: [SayWorld]
  errors: [NotAuthorizedError]
}


@readonly
@http(method: "GET", uri: "/hello", code: 200)
operation SayWorld {
  output: World
}

structure World {
  message: String = "World !"
}

@error("client")
@httpError(401)
structure NotAuthorizedError {
  @required
  message: String
}
