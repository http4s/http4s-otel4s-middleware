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

private[metrics] object MetricsConfigDefaults {
  val DurationHistogramBuckets: BucketBoundaries =
    BucketBoundaries(.005, .01, .025, .05, .075, .1, .25, .5, .75, 1, 2.5, 5, 7.5, 10)

  val BodySizeHistogramBuckets: BucketBoundaries =
    BucketBoundaries(0, 64, 128, 256, 512, 1024, 4096, 16384, 65536, 262144, 1048576)
}
