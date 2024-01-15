package org.http4s.otel4s

import cats.effect.Sync
import cats.syntax.all._
import org.http4s.metrics.TerminationType.{Abnormal, Canceled, Error, Timeout}
import org.http4s.metrics.{MetricsOps, TerminationType}
import org.http4s.{Method, Status}
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.metrics.{Counter, Histogram, Meter, UpDownCounter}

/** [[MetricsOps]] algebra capable of recording OpenTelemetry metrics
 *
 * Registers the following metrics:
 *
 * {prefix}.response.duration{labels=classifier,method,phase} - Histogram
 *
 * {prefix}.active_request.count{labels=classifier} - Gauge
 *
 * {prefix}.request.count{labels=classifier,method,status} - Counter
 *
 * {prefix}.abnormal_terminations{labels=classifier,termination_type} - Histogram
 *
 * Labels --
 *
 * method: Enumeration values: get, put, post, head, move, options, trace, connect, delete, other
 *
 * phase: Enumeration values: headers, body
 *
 * code: Enumeration values: 1xx, 2xx, 3xx, 4xx, 5xx
 *
 * termination_type: Enumeration values: abnormal, error, timeout
 */
object OtelMetrics {

  /** Creates a [[MetricsOps]] that supports OpenTelemetry metrics
   *
   * @param prefix
   * a prefix that will be added to all metrics
   */
  def metricsOps[F[_] : Sync : Meter](prefix: String = "org.http4s.server"): F[MetricsOps[F]] =
    for {
      metrics <- createMetricsCollection(prefix)
    } yield createMetricsOps(metrics)

  private def createMetricsOps[F[_] : Sync](metrics: MetricsCollection[F]): MetricsOps[F] =
    new MetricsOps[F] {
      override def increaseActiveRequests(classifier: Option[String]): F[Unit] =
        metrics.activeRequests
          .inc(Attribute("classifier", label(classifier)))

      override def decreaseActiveRequests(classifier: Option[String]): F[Unit] =
        metrics.activeRequests
          .dec(Attribute("classifier", label(classifier)))

      override def recordHeadersTime(
                                      method: Method,
                                      elapsed: Long,
                                      classifier: Option[String]
                                    ): F[Unit] =
        metrics.responseDuration
          .record(
            secondsFromNanos(elapsed),
            Attribute("classifier", label(classifier)),
            Attribute("method", reportMethod(method)),
            Attribute("phase", Phase.report(Phase.Headers))
          )

      override def recordTotalTime(
                                    method: Method,
                                    status: Status,
                                    elapsed: Long,
                                    classifier: Option[String]
                                  ): F[Unit] =
        metrics.responseDuration
          .record(
            secondsFromNanos(elapsed),
            Attribute("classifier", label(classifier)),
            Attribute("method", reportMethod(method)),
            Attribute("phase", Phase.report(Phase.Body))
          ) *>
          metrics.requests
            .inc(
              Attribute("classifier", label(classifier)),
              Attribute("method", reportMethod(method)),
              Attribute("status", reportStatus(status))
            )

      override def recordAbnormalTermination(
                                              elapsed: Long,
                                              terminationType: TerminationType,
                                              classifier: Option[String]
                                            ): F[Unit] =
        terminationType match {
          case Abnormal(e) => recordAbnormal(elapsed, classifier, e)
          case Error(e) => recordError(elapsed, classifier, e)
          case Canceled => recordCanceled(elapsed, classifier)
          case Timeout => recordTimeout(elapsed, classifier)
        }

      private def recordCanceled(elapsed: Long, classifier: Option[String]): F[Unit] =
        metrics.abnormalTerminations
          .record(
            secondsFromNanos(elapsed),
            Attribute("classifier", label(classifier)),
            Attribute("termination_type", AbnormalTermination.report(AbnormalTermination.Canceled)),
            Attribute("cause", label(Option.empty))
          )

      private def recordAbnormal(
                                  elapsed: Long,
                                  classifier: Option[String],
                                  cause: Throwable
                                ): F[Unit] =
        metrics.abnormalTerminations
          .record(
            secondsFromNanos(elapsed),
            Attribute("classifier", label(classifier)),
            Attribute("termination_type", AbnormalTermination.report(AbnormalTermination.Abnormal)),
            Attribute("cause", label(Option(cause.getClass.getName)))
          )

      private def recordError(
                               elapsed: Long,
                               classifier: Option[String],
                               cause: Throwable
                             ): F[Unit] =
        metrics.abnormalTerminations
          .record(
            secondsFromNanos(elapsed),
            Attribute("classifier", label(classifier)),
            Attribute("termination_type", AbnormalTermination.report(AbnormalTermination.Error)),
            Attribute("cause", label(Option(cause.getClass.getName)))
          )

      private def recordTimeout(elapsed: Long, classifier: Option[String]): F[Unit] =
        metrics.abnormalTerminations
          .record(
            secondsFromNanos(elapsed),
            Attribute("classifier", label(classifier)),
            Attribute("termination_type", AbnormalTermination.report(AbnormalTermination.Timeout)),
            Attribute("cause", label(Option.empty))
          )

      private def secondsFromNanos(nanos: Long): Double =
        nanos / 1_000_000_000.0

      private def label(value: Option[String]): String = value.getOrElse("")

      private def reportStatus(status: Status): String =
        status.code match {
          case hundreds if hundreds < 200 => "1xx"
          case twohundreds if twohundreds < 300 => "2xx"
          case threehundreds if threehundreds < 400 => "3xx"
          case fourhundreds if fourhundreds < 500 => "4xx"
          case _ => "5xx"
        }

      private def reportMethod(m: Method): String =
        m match {
          case Method.GET => "get"
          case Method.PUT => "put"
          case Method.POST => "post"
          case Method.PATCH => "patch"
          case Method.HEAD => "head"
          case Method.MOVE => "move"
          case Method.OPTIONS => "options"
          case Method.TRACE => "trace"
          case Method.CONNECT => "connect"
          case Method.DELETE => "delete"
          case _ => "other"
        }
    }

  private def createMetricsCollection[F[_] : Sync : Meter](prefix: String): F[MetricsCollection[F]] = {
    val responseDuration: F[Histogram[F, Double]] = {
      Meter[F]
        .histogram(prefix + ".response.duration")
        .withUnit("seconds")
        .withDescription("Response Duration in seconds.")
        .create
    }

    val activeRequests: F[UpDownCounter[F, Long]] = {
      Meter[F]
        .upDownCounter(prefix + ".active_request.count")
        .withDescription("Total Active Requests.")
        .create
    }

    val requests: F[Counter[F, Long]] = {
      Meter[F]
        .counter(prefix + ".request.count")
        .withDescription("Total Requests.")
        .create
    }

    val abnormalTerminations: F[Histogram[F, Double]] = {
      Meter[F]
        .histogram(prefix + ".abnormal_terminations")
        .withDescription("Total Abnormal Terminations.")
        .create
    }

    (responseDuration, activeRequests, requests, abnormalTerminations).mapN(MetricsCollection.apply)
  }

  val ResponseDurationDefaultHistogramBuckets: List[Double] =
    List(.005, .01, .025, .05, .075, .1, .25, .5, .75, 1, 2.5, 5, 7.5, 10)
}

final case class MetricsCollection[F[_]](
                                          responseDuration: Histogram[F, Double],
                                          activeRequests: UpDownCounter[F, Long],
                                          requests: Counter[F, Long],
                                          abnormalTerminations: Histogram[F, Double]
                                        )

private sealed trait Phase

private object Phase {
  case object Headers extends Phase

  case object Body extends Phase

  def report(s: Phase): String =
    s match {
      case Headers => "headers"
      case Body => "body"
    }
}

private sealed trait AbnormalTermination

private object AbnormalTermination {
  case object Abnormal extends AbnormalTermination

  case object Error extends AbnormalTermination

  case object Timeout extends AbnormalTermination

  case object Canceled extends AbnormalTermination

  def report(t: AbnormalTermination): String =
    t match {
      case Abnormal => "abnormal"
      case Timeout => "timeout"
      case Error => "error"
      case Canceled => "cancel"
    }
}
