/*
 * Copyright 2023 http4s.org
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 */

package org.http4s
package otel4s.middleware.metrics

import org.http4s.metrics.MetricsRequest

/** Selects which HTTP requests are recorded by the metrics middleware. */
trait MetricsRequestFilter {

  /** Returns true when metrics should be recorded for `request`. */
  def apply(request: MetricsRequest): Boolean
}

object MetricsRequestFilter {

  /** Records every request. */
  val all: MetricsRequestFilter = new MetricsRequestFilter {
    def apply(request: MetricsRequest): Boolean = true
  }

  /** Records no requests. */
  val none: MetricsRequestFilter = new MetricsRequestFilter {
    def apply(request: MetricsRequest): Boolean = false
  }

  /** Creates a filter from a predicate. */
  def apply(predicate: MetricsRequest => Boolean): MetricsRequestFilter =
    new MetricsRequestFilter {
      def apply(request: MetricsRequest): Boolean = predicate(request)
    }
}
