/*
 * Copyright 2023 http4s.org
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.http4s.otel4s.middleware.trace
package server

import cats.effect.IO
import org.http4s.HttpApp
import org.http4s.Request
import org.typelevel.otel4s.sdk.trace.data.SpanData
import org.typelevel.otel4s.trace.SpanKind
import org.typelevel.otel4s.trace.Tracer

class ServerMiddlewareTest extends MiddlewareTest[ServerMiddleware.Builder, HttpApp] {
  protected def middlewareBuilder(implicit tracer: Tracer[IO]): ServerMiddleware.Builder[IO] =
    ServerMiddleware.builder
  protected def build(builder: ServerMiddleware.Builder[IO], app: HttpApp[IO]): HttpApp[IO] =
    builder.buildHttpApp(app)
  protected def runRequest(traced: HttpApp[IO], request: Request[IO]): IO[Unit] =
    traced.run(request).void
  protected def checkSpanNameAndKind(span: SpanData): Unit = {
    assertEquals(span.name, "Http Server - GET")
    assertEquals(span.kind, SpanKind.Server)
  }

  testMiddleware("ServerMiddleware")
}
