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

package org.http4s.otel4s.middleware.metrics

import org.typelevel.otel4s.metrics.BucketBoundaries

/** Body-size source and histogram boundaries in bytes. */
sealed trait BodySizeConfig {
  def strategy: BodySizeConfig.Strategy
  def bucketBoundaries: BucketBoundaries
}

object BodySizeConfig {

  val DefaultBucketBoundaries: BucketBoundaries =
    BucketBoundaries(
      0d, 1024d, 2048d, 4096d, 8192d, 16384d, 32768d, 65536d, 131072d, 262144d, 524288d, 1048576d,
      2097152d, 4194304d, 8388608d,
    )

  sealed trait Strategy
  object Strategy {
    case object ContentLength extends Strategy
    case object Observe extends Strategy
  }

  /** Uses the declared length, even for unread or partially consumed bodies. Missing lengths are omitted. */
  def contentLength: BodySizeConfig = contentLength(DefaultBucketBoundaries)

  /** Uses the declared length with the given histogram boundaries in bytes. */
  def contentLength(bucketBoundaries: BucketBoundaries): BodySizeConfig =
    Impl(Strategy.ContentLength, bucketBoundaries)

  /** Counts the first body evaluation. Incomplete observations are omitted. */
  def observe: BodySizeConfig = observe(DefaultBucketBoundaries)

  /** Counts the first body evaluation with the given histogram boundaries in bytes. */
  def observe(bucketBoundaries: BucketBoundaries): BodySizeConfig =
    Impl(Strategy.Observe, bucketBoundaries)

  private final case class Impl(
      strategy: Strategy,
      bucketBoundaries: BucketBoundaries,
  ) extends BodySizeConfig
}
