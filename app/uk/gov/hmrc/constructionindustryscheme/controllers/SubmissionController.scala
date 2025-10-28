/*
 * Copyright 2025 HM Revenue & Customs
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

package uk.gov.hmrc.constructionindustryscheme.controllers


import javax.inject.Inject
import play.api.mvc.*
import play.api.libs.json.*

import scala.concurrent.{ExecutionContext, Future}
import play.api.Logging
import play.api.http.Status.BAD_GATEWAY
import uk.gov.hmrc.constructionindustryscheme.actions.AuthAction
import uk.gov.hmrc.constructionindustryscheme.models.{BuiltSubmissionPayload, SubmissionResult, ACCEPTED as AcceptedStatus, DEPARTMENTAL_ERROR as DepartmentalErrorStatus, FATAL_ERROR as FatalErrorStatus, SUBMITTED as SubmittedStatus, SUBMITTED_NO_RECEIPT as SubmittedNoReceiptStatus}
import uk.gov.hmrc.constructionindustryscheme.config.AppConfig
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController
import uk.gov.hmrc.constructionindustryscheme.models.requests.{ChrisSubmissionRequest, CreateSubmissionRequest, UpdateSubmissionRequest}
import uk.gov.hmrc.constructionindustryscheme.services.{AuditService, SubmissionService}
import uk.gov.hmrc.constructionindustryscheme.services.chris.ChrisEnvelopeBuilder
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.constructionindustryscheme.models.audit.{AuditResponseReceivedModel, XmlConversionResult}
import uk.gov.hmrc.constructionindustryscheme.utils.XmlToJsonConvertor

import java.time.{Clock, Instant}
import java.nio.charset.Charset
import java.util.UUID

class SubmissionController @Inject()(
                                           authorise: AuthAction,
                                           submissionService: SubmissionService,
                                           auditService: AuditService,
                                           cc: ControllerComponents,
                                           appConfig: AppConfig,
                                           clock: Clock
                                         )(implicit ec: ExecutionContext)
  extends BackendController(cc) with Logging {

  implicit val reads: Reads[ChrisSubmissionRequest] = Json.reads[ChrisSubmissionRequest]

  def createSubmission: Action[JsValue] =
    authorise(parse.json).async { implicit request =>
      request.body.validate[CreateSubmissionRequest].fold(
        errs => Future.successful(BadRequest(JsError.toJson(errs))),
        csr =>
          submissionService
            .createSubmission(csr)
            .map(id => Created(Json.obj("submissionId" -> id)))
            .recover { case ex =>
              logger.error("[create] formp-proxy create failed", ex)
              BadGateway(Json.obj("message" -> "create-submission-failed"))
            }
      )
    }

  def submitToChris(submissionId: String): Action[JsValue] =
    authorise(parse.json).async { implicit req =>
      req.body.validate[ChrisSubmissionRequest].fold(
        errs => Future.successful(BadRequest(Json.obj("message" -> JsError.toJson(errs)))),
        csr => {
          logger.info(s"Submitting Nil Monthly Return to ChRIS for UTR=${csr.utr}")

          val correlationId = UUID.randomUUID().toString.replace("-", "").toUpperCase
          val payload = ChrisEnvelopeBuilder.buildPayload(csr, req, correlationId, appConfig.chrisEnableMissingMandatory, appConfig.chrisEnableIrmarkBad)

          val monthlyNilReturnRequestJson: JsValue = createMonthlyNilReturnRequestJson(payload)
          auditService.monthlyNilReturnRequestEvent(monthlyNilReturnRequestJson)

          submissionService
            .submitToChris(payload)
            .map(renderSubmissionResponse(submissionId, payload))
            .recover { case ex =>
              logger.error("[submitToChris] upstream failure", ex)
              val fatalErrorJson = Json.obj(
                "submissionId" -> submissionId,
                "status" -> "FATAL_ERROR",
                "hmrcMarkGenerated" -> payload.irMark,
                "error" -> "upstream-failure"
              )
              val monthlyNilReturnResponse = AuditResponseReceivedModel(BAD_GATEWAY.toString, fatalErrorJson)
              auditService.monthlyNilReturnResponseEvent(monthlyNilReturnResponse)
              BadGateway(fatalErrorJson)
            }
        }
      )
    }

  def updateSubmission(submissionId: String): Action[JsValue] =
    authorise(parse.json).async { implicit req =>
      req.body.validate[UpdateSubmissionRequest].fold(
        e => Future.successful(BadRequest(JsError.toJson(e))),
        upd => {
          submissionService.updateSubmission(upd).map { _ =>
            NoContent
          }.recover { case ex =>
            logger.error("[updateSubmission] formp-proxy update failed", ex)
            BadGateway(Json.obj("submissionId" -> submissionId, "message" -> "update-submission-failed"))
          }
        }
      )
    }

  def pollSubmission(pollUrl: String, correlationId: String): Action[AnyContent] =
    authorise.async { implicit req =>
      val timestamp = Instant.parse(java.net.URLDecoder.decode(pollUrl, Charset.forName("UTF-8")).split('=').apply(1))

      val taxOfficeNumber = req.enrolments
        .getEnrolment("HMRC-CIS-ORG")
        .flatMap(_.getIdentifier("TaxOfficeNumber"))
        .map(_.value)

      if (taxOfficeNumber.contains("754") && Instant.now.isAfter(timestamp.plusSeconds(15))) {
        Future.successful(Ok(Json.obj(
          "status" -> "SUBMITTED"
        )))
      } else if (taxOfficeNumber.contains("755") && Instant.now.isAfter(timestamp.plusSeconds(15))) {
        Future.successful(Ok(Json.obj(
          "status" -> "FATAL_ERROR"
        )))
      }
      else if (taxOfficeNumber.contains("756") && Instant.now.isAfter(timestamp.plusSeconds(15))) {
        Future.successful(Ok(Json.obj(
          "status" -> "DEPARTMENTAL_ERROR"
        )))
      } else {
        Future.successful(Ok(Json.obj(
          "status" -> "PENDING",
          "pollUrl" -> pollUrl
        )))
      }
    }

  private def renderSubmissionResponse(submissionId: String, payload: BuiltSubmissionPayload)(res: SubmissionResult): Result = {

    implicit val hc: HeaderCarrier = HeaderCarrier()

    val gatewayTimestamp: String = res.meta.gatewayTimestamp match {
      case Some(s) if s.trim.nonEmpty => s.trim
      case _ => Instant.now(clock).toString
    }
    val base = Json.obj(
      "submissionId" -> submissionId,
      "hmrcMarkGenerated" -> payload.irMark,
      "correlationId" -> res.meta.correlationId,
      "gatewayTimestamp" -> gatewayTimestamp
    )

    def withStatus(s: String): JsObject = base ++ Json.obj("status" -> s)

    def withPoll(o: JsObject): JsObject = {
      val endpoint = res.meta.responseEndPoint
      o ++ Json.obj(
        "responseEndPoint" -> Json.obj(
          "url" -> s"http://someurl.com/test?timestamp=${Instant.now}",  // endpoint.url,
          "pollIntervalSeconds" -> endpoint.pollIntervalSeconds
        )
      )
    }

    def errorObj(defaultText: String): JsObject =
      Json.obj("error" ->
        res.meta.error
          .map(e => Json.obj("number" -> e.errorNumber, "type" -> e.errorType, "text" -> e.errorText))
          .getOrElse(Json.obj("text" -> defaultText))
      )

    val monthlyNilReturnResponseJson: JsValue = createMonthlyNilReturnResponseJson(res)

    val monthlyNilReturnResponse = AuditResponseReceivedModel(res.status.toString, monthlyNilReturnResponseJson)
    auditService.monthlyNilReturnResponseEvent(monthlyNilReturnResponse)

    res.status match {
      case AcceptedStatus => Results.Accepted(withPoll(withStatus("ACCEPTED")))
      case SubmittedStatus => Results.Ok(withStatus("SUBMITTED"))
      case SubmittedNoReceiptStatus => Results.Ok(withStatus("SUBMITTED_NO_RECEIPT"))
      case DepartmentalErrorStatus => Results.Ok(withStatus("DEPARTMENTAL_ERROR") ++ errorObj("departmental error"))
      case FatalErrorStatus => Results.Ok(withStatus("FATAL_ERROR") ++ errorObj("fatal"))
    }
  }

  def createMonthlyNilReturnRequestJson(payload: BuiltSubmissionPayload): JsValue = {
    XmlToJsonConvertor.convertXmlToJson(payload.envelope.toString) match {
      case XmlConversionResult(true, Some(json), _) => json
      case XmlConversionResult(false, _, Some(error)) => Json.obj("error" -> error)
      case _ => Json.obj("error" -> "unexpected conversion failure")
    }
  }

  def createMonthlyNilReturnResponseJson(res: SubmissionResult): JsValue = {
    XmlToJsonConvertor.convertXmlToJson(res.rawXml) match {
      case XmlConversionResult(true, Some(json), _) => json
      case _ => Json.toJson(res.rawXml)
    }
  }

}
