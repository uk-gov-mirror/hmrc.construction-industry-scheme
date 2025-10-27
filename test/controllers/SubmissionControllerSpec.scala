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

package controllers

import base.SpecBase
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.*
import org.mockito.{ArgumentMatchers, Mockito}
import org.scalatest.EitherValues
import play.api.http.Status.{BAD_GATEWAY, BAD_REQUEST, CREATED, NO_CONTENT, OK, UNAUTHORIZED}
import play.api.libs.json.{JsObject, JsValue, Json}
import play.api.test.FakeRequest
import play.api.test.Helpers.{CONTENT_TYPE, GET, JSON, POST, contentAsJson, status}
import uk.gov.hmrc.constructionindustryscheme.actions.AuthAction
import uk.gov.hmrc.constructionindustryscheme.config.AppConfig
import uk.gov.hmrc.constructionindustryscheme.controllers.SubmissionController
import uk.gov.hmrc.constructionindustryscheme.models.audit.XmlConversionResult
import uk.gov.hmrc.constructionindustryscheme.models.requests.{CreateSubmissionRequest, UpdateSubmissionRequest}
import uk.gov.hmrc.constructionindustryscheme.models.{ACCEPTED, BuiltSubmissionPayload, DEPARTMENTAL_ERROR, GovTalkError, GovTalkMeta, ResponseEndPoint, SUBMITTED, SUBMITTED_NO_RECEIPT, SubmissionResult, SubmissionStatus}
import uk.gov.hmrc.constructionindustryscheme.services.{AuditService, SubmissionService}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.audit.http.connector.AuditResult

import java.time.{Instant, Clock}
import scala.concurrent.Future


final class SubmissionControllerSpec extends SpecBase with EitherValues {

  private val submissionId = "sub-123"

  private val validJson: JsValue = Json.obj(
    "utr" -> "1234567890",
    "aoReference" -> "123/AB456",
    "informationCorrect" -> "yes",
    "inactivity" -> "yes",
    "monthYear" -> "2025-09"
  )

  val mockAuditService: AuditService = mock[AuditService]

  override def beforeEach(): Unit = {
    Mockito.reset(
      mockAuditService
    )
    super.beforeEach()
  }

  private def mkAppConfig(
                           missingMandatory: Boolean = false,
                           irmarkBad: Boolean = false
                         ): AppConfig = {
    val appConfig = mock[AppConfig]
    when(appConfig.chrisEnableMissingMandatory).thenReturn(missingMandatory)
    when(appConfig.chrisEnableIrmarkBad).thenReturn(irmarkBad)
    appConfig
  }

  private def mkController(
                            service: SubmissionService,
                            auth: AuthAction = fakeAuthAction(),
                            appConfig: AppConfig = mkAppConfig(),
                            clock: Clock = Clock.systemUTC()
                          ): SubmissionController =
    new SubmissionController(auth, service, mockAuditService, cc, appConfig, clock)

  "submitToChris" - {

    "returns 200 with SUBMITTED when service returns SubmittedStatus" in {
      val service    = mock[SubmissionService]
      val controller = mkController(service)

      when(mockAuditService.monthlyNilReturnRequestEvent(any())(any())).thenReturn(Future.successful(AuditResult.Success))
      when(mockAuditService.monthlyNilReturnResponseEvent(any())(any())).thenReturn(Future.successful(AuditResult.Success))
      when(service.submitToChris(any[BuiltSubmissionPayload])(any[HeaderCarrier]))
        .thenReturn(Future.successful(mkSubmissionResult(SUBMITTED)))

      val request: FakeRequest[JsValue] =
        FakeRequest(POST, s"/cis/submissions/$submissionId/submit-to-chris")
          .withBody(validJson)
          .withHeaders(CONTENT_TYPE -> JSON)

      val result= controller.submitToChris(submissionId)(request)

      status(result) mustBe OK
      val js = contentAsJson(result)
      (js \ "submissionId").as[String] mustBe submissionId

      verify(mockAuditService, times(1)).monthlyNilReturnRequestEvent(any())(any())
      verify(mockAuditService, times(1)).monthlyNilReturnResponseEvent(any())(any())
      verify(service, times(1)).submitToChris(any[BuiltSubmissionPayload])(any[HeaderCarrier])
    }
    
    "returns 200 with SUBMITTED_NO_RECEIPT when service returns SubmittedNoReceiptStatus" in {
      val service    = mock[SubmissionService]
      val controller = mkController(service)

      when(mockAuditService.monthlyNilReturnRequestEvent(any())(any())).thenReturn(Future.successful(AuditResult.Success))
      when(mockAuditService.monthlyNilReturnResponseEvent(any())(any())).thenReturn(Future.successful(AuditResult.Success))
      when(service.submitToChris(any[BuiltSubmissionPayload])(any[HeaderCarrier]))
        .thenReturn(Future.successful(mkSubmissionResult(SUBMITTED_NO_RECEIPT)))

      val req = FakeRequest(POST, s"/cis/submissions/$submissionId/submit-to-chris")
        .withBody(validJson)
        .withHeaders(CONTENT_TYPE -> JSON)

      val result = controller.submitToChris(submissionId)(req)

      status(result) mustBe OK

      verify(mockAuditService, times(1)).monthlyNilReturnRequestEvent(any())(any())
      verify(mockAuditService, times(1)).monthlyNilReturnResponseEvent(any())(any())
      val js = contentAsJson(result)
      (js \ "status").as[String] mustBe "SUBMITTED_NO_RECEIPT"
      (js \ "submissionId").as[String] mustBe submissionId
    }

    "returns 202 with ACCEPTED when service returns AcceptedStatus (includes nextPollInSeconds)" in {
      val service    = mock[SubmissionService]
      val controller = mkController(service)

      when(mockAuditService.monthlyNilReturnRequestEvent(any())(any())).thenReturn(Future.successful(AuditResult.Success))
      when(mockAuditService.monthlyNilReturnResponseEvent(any())(any())).thenReturn(Future.successful(AuditResult.Success))
      when(service.submitToChris(any[BuiltSubmissionPayload])(any[HeaderCarrier]))
        .thenReturn(Future.successful(mkSubmissionResult(ACCEPTED)))

      val req = FakeRequest(POST, s"/cis/submissions/$submissionId/submit-to-chris")
        .withBody(validJson)
        .withHeaders(CONTENT_TYPE -> JSON)

      val result = controller.submitToChris(submissionId)(req)

      status(result) mustBe 202
      verify(mockAuditService, times(1)).monthlyNilReturnRequestEvent(any())(any())
      verify(mockAuditService, times(1)).monthlyNilReturnResponseEvent(any())(any())
      val js = contentAsJson(result)
      (js \ "status").as[String] mustBe "ACCEPTED"
      (js \ "responseEndPoint" \ "pollIntervalSeconds").as[Int] mustBe 15
    }

    "returns 200 with DEPARTMENTAL_ERROR and error object when service returns DepartmentalErrorStatus" in {
      val service    = mock[SubmissionService]
      val controller = mkController(service)

      val err = GovTalkError("1234", "fatal", "boom")
      when(mockAuditService.monthlyNilReturnRequestEvent(any())(any())).thenReturn(Future.successful(AuditResult.Success))
      when(mockAuditService.monthlyNilReturnResponseEvent(any())(any())).thenReturn(Future.successful(AuditResult.Success))
      when(service.submitToChris(any[BuiltSubmissionPayload])(any[HeaderCarrier]))
        .thenReturn(Future.successful(mkSubmissionResult(DEPARTMENTAL_ERROR, Some(err))))

      val req = FakeRequest(POST, s"/cis/submissions/$submissionId/submit-to-chris")
        .withBody(validJson)
        .withHeaders(CONTENT_TYPE -> JSON)

      val result = controller.submitToChris(submissionId)(req)

      status(result) mustBe OK
      verify(mockAuditService, times(1)).monthlyNilReturnRequestEvent(any())(any())
      verify(mockAuditService, times(1)).monthlyNilReturnResponseEvent(any())(any())
      val js = contentAsJson(result)
      (js \ "status").as[String] mustBe "DEPARTMENTAL_ERROR"
      val e = (js \ "error").as[JsObject]
      (e \ "number").as[String] mustBe "1234"
      (e \ "type").as[String] mustBe "fatal"
      (e \ "text").as[String].toLowerCase must include ("boom")
    }

    "returns 400 when request JSON is invalid" in {
      val service    = mock[SubmissionService]
      val controller = mkController(service)

      when(mockAuditService.monthlyNilReturnRequestEvent(any())(any())).thenReturn(Future.successful(AuditResult.Success))

      val badJson = Json.obj("utr" -> 123)

      val req = FakeRequest(POST, s"/cis/submissions/$submissionId/submit-to-chris")
        .withBody(badJson)
        .withHeaders(CONTENT_TYPE -> JSON)

      val result = controller.submitToChris(submissionId)(req)

      status(result) mustBe BAD_REQUEST
      (contentAsJson(result) \ "message").isDefined mustBe true

      verifyNoInteractions(mockAuditService)
      verifyNoInteractions(service)
    }

    "returns 502 BadGateway when service fails" in {
      val service    = mock[SubmissionService]
      val controller = mkController(service)

      when(mockAuditService.monthlyNilReturnRequestEvent(any())(any())).thenReturn(Future.successful(AuditResult.Success))
      when(mockAuditService.monthlyNilReturnResponseEvent(any())(any())).thenReturn(Future.successful(AuditResult.Success))
      when(service.submitToChris(any[BuiltSubmissionPayload])(any[HeaderCarrier]))
        .thenReturn(Future.failed(new RuntimeException("boom")))

      val req = FakeRequest(POST, s"/cis/submissions/$submissionId/submit-to-chris")
        .withBody(validJson)
        .withHeaders(CONTENT_TYPE -> JSON)

      val result = controller.submitToChris(submissionId)(req)

      status(result) mustBe BAD_GATEWAY
      val js = contentAsJson(result)
      (js \ "submissionId").as[String] mustBe submissionId
      (js \ "status").as[String] mustBe "FATAL_ERROR"
      (js \ "error").as[String] mustBe "upstream-failure"

      verify(mockAuditService, times(1)).monthlyNilReturnRequestEvent(any())(any())
      verify(mockAuditService, times(1)).monthlyNilReturnResponseEvent(any())(any())
      verify(service, times(1)).submitToChris(any[BuiltSubmissionPayload])(any[HeaderCarrier])
    }
  }

  "createSubmission" - {

    val validCreateJson = Json.obj(
      "instanceId" -> "123",
      "taxYear" -> 2024,
      "taxMonth" -> 4
    )

    "returns 201 with submissionId when service returns id" in {
      val service = mock[SubmissionService]
      val controller = mkController(service)

      when(service.createSubmission(any[CreateSubmissionRequest])(any[HeaderCarrier]))
        .thenReturn(Future.successful("sub-999"))

      val req = FakeRequest(POST, "/cis/submissions/create-and-track")
        .withBody(validCreateJson)
        .withHeaders(CONTENT_TYPE -> JSON)

      val result = controller.createSubmission(req)

      status(result) mustBe CREATED
      (contentAsJson(result) \ "submissionId").as[String] mustBe "sub-999"

      verify(service).createSubmission(any[CreateSubmissionRequest])(any[HeaderCarrier])
    }

    "returns 400 when JSON is invalid" in {
      val service = mock[SubmissionService]
      val controller = mkController(service)

      val bad = Json.obj("taxYear" -> 2024)

      val req = FakeRequest(POST, "/cis/submissions/create-and-track")
        .withBody(bad)
        .withHeaders(CONTENT_TYPE -> JSON)

      val result = controller.createSubmission(req)

      status(result) mustBe BAD_REQUEST
      contentAsJson(result).toString must include("obj.instanceId")
      verifyNoInteractions(service)
    }

    "returns 502 when service fails" in {
      val service = mock[SubmissionService]
      val controller = mkController(service)

      when(service.createSubmission(any[CreateSubmissionRequest])(any[HeaderCarrier]))
        .thenReturn(Future.failed(new RuntimeException("formp down")))

      val req = FakeRequest(POST, "/cis/submissions/create-and-track")
        .withBody(validCreateJson)
        .withHeaders(CONTENT_TYPE -> JSON)

      val result = controller.createSubmission(req)

      status(result) mustBe BAD_GATEWAY
      (contentAsJson(result) \ "message").as[String] mustBe "create-submission-failed"
    }

    "returns 401 when unauthorised" in {
      val service = mock[SubmissionService]
      val controller = mkController(service, auth = rejectingAuthAction)

      val req = FakeRequest(POST, "/cis/submissions/create-and-track")
        .withBody(validCreateJson)
        .withHeaders(CONTENT_TYPE -> JSON)

      val result = controller.createSubmission(req)

      status(result) mustBe UNAUTHORIZED
      verifyNoInteractions(service)
    }
  }

  "updateSubmission" - {

    val minimalUpdateJson = Json.obj(
      "instanceId" -> "123",
      "taxYear" -> 2024,
      "taxMonth" -> 4,
      "submittableStatus" -> "REJECTED"
    )

    "returns 204 NoContent when service updates ok" in {
      val service = mock[SubmissionService]
      val controller = mkController(service)

      when(service.updateSubmission(any[UpdateSubmissionRequest])(any[HeaderCarrier]))
        .thenReturn(Future.unit)

      val req = FakeRequest(POST, s"/cis/submissions/$submissionId/update")
        .withBody(minimalUpdateJson)
        .withHeaders(CONTENT_TYPE -> JSON)

      val result = controller.updateSubmission(submissionId)(req)

      status(result) mustBe NO_CONTENT
      verify(service).updateSubmission(any[UpdateSubmissionRequest])(any[HeaderCarrier])
    }

    "returns 400 when JSON is invalid" in {
      val service = mock[SubmissionService]
      val controller = mkController(service)

      val bad = Json.obj("instanceId" -> "123")

      val req = FakeRequest(POST, s"/cis/submissions/$submissionId/update")
        .withBody(bad)
        .withHeaders(CONTENT_TYPE -> JSON)

      val result = controller.updateSubmission(submissionId)(req)

      status(result) mustBe BAD_REQUEST
      verifyNoInteractions(service)
    }

    "returns 502 BadGateway when service fails" in {
      val service = mock[SubmissionService]
      val controller = mkController(service)

      when(service.updateSubmission(any[UpdateSubmissionRequest])(any[HeaderCarrier]))
        .thenReturn(Future.failed(new RuntimeException("formp update failed")))

      val req = FakeRequest(POST, s"/cis/submissions/$submissionId/update")
        .withBody(minimalUpdateJson)
        .withHeaders(CONTENT_TYPE -> JSON)

      val result = controller.updateSubmission(submissionId)(req)

      status(result) mustBe BAD_GATEWAY
      val js = contentAsJson(result)
      (js \ "submissionId").as[String] mustBe submissionId
      (js \ "message").as[String] mustBe "update-submission-failed"
    }

    "returns 401 when unauthorised" in {
      val service = mock[SubmissionService]
      val controller = mkController(service, auth = rejectingAuthAction)

      val req = FakeRequest(POST, s"/cis/submissions/$submissionId/update")
        .withBody(minimalUpdateJson)
        .withHeaders(CONTENT_TYPE -> JSON)

      val result = controller.updateSubmission(submissionId)(req)

      status(result) mustBe UNAUTHORIZED
      verifyNoInteractions(service)
    }
  }

  "pollSubmission" - {

    "returns 200 with SUBMITTED status when timestamp is more than 25 seconds in the past" in {
      val service = mock[SubmissionService]
      val authAction = fakeAuthAction(ton = "EZ001", tor = "EZ00100")
      val controller = mkController(service, auth = authAction)

      val oldTimestamp = Instant.now().minusSeconds(30)
      val encodedPollUrl = java.net.URLEncoder.encode(s"http://example.com/poll?timestamp=$oldTimestamp", "UTF-8")
      val correlationId = "CORR123"

      val req = FakeRequest(GET, s"/cis/submissions/poll?pollUrl=$encodedPollUrl&correlationId=$correlationId")

      val result = controller.pollSubmission(encodedPollUrl, correlationId)(req)

      status(result) mustBe OK
      val js = contentAsJson(result)
      (js \ "status").as[String] mustBe "SUBMITTED"
      (js \ "pollUrl").asOpt[String] mustBe None
      verifyNoInteractions(service)
    }

    "returns 200 with PENDING status and pollUrl when timestamp is less than 25 seconds in the past" in {
      val service = mock[SubmissionService]
      val authAction = fakeAuthAction(ton = "EZ001", tor = "EZ00100")
      val controller = mkController(service, auth = authAction)

      val recentTimestamp = Instant.now().minusSeconds(10)
      val encodedPollUrl = java.net.URLEncoder.encode(s"http://example.com/poll?timestamp=$recentTimestamp", "UTF-8")
      val correlationId = "CORR789"

      val req = FakeRequest(GET, s"/cis/submissions/poll?pollUrl=$encodedPollUrl&correlationId=$correlationId")

      val result = controller.pollSubmission(encodedPollUrl, correlationId)(req)

      status(result) mustBe OK
      val js = contentAsJson(result)
      (js \ "status").as[String] mustBe "PENDING"
      (js \ "pollUrl").as[String] mustBe encodedPollUrl
      verifyNoInteractions(service)
    }

    "returns 200 with PENDING status when timestamp is in the future" in {
      val service = mock[SubmissionService]
      val authAction = fakeAuthAction(ton = "EZ001", tor = "EZ00100")
      val controller = mkController(service, auth = authAction)

      val futureTimestamp = Instant.now().plusSeconds(60)
      val encodedPollUrl = java.net.URLEncoder.encode(s"http://example.com/poll?timestamp=$futureTimestamp", "UTF-8")
      val correlationId = "CORR999"

      val req = FakeRequest(GET, s"/cis/submissions/poll?pollUrl=$encodedPollUrl&correlationId=$correlationId")

      val result = controller.pollSubmission(encodedPollUrl, correlationId)(req)

      status(result) mustBe OK
      val js = contentAsJson(result)
      (js \ "status").as[String] mustBe "PENDING"
      (js \ "pollUrl").as[String] mustBe encodedPollUrl
      verifyNoInteractions(service)
    }

    "returns 200 with FATAL_ERROR when TaxOfficeReference does not match expected value" in {
      val service = mock[SubmissionService]
      val authAction = fakeAuthAction(ton = "123", tor = "AB456")
      val controller = mkController(service, auth = authAction)

      val oldTimestamp = Instant.now().minusSeconds(30)
      val encodedPollUrl = java.net.URLEncoder.encode(s"http://example.com/poll?timestamp=$oldTimestamp", "UTF-8")
      val correlationId = "CORR-BAD-TOR"

      val req = FakeRequest(GET, s"/cis/submissions/poll?pollUrl=$encodedPollUrl&correlationId=$correlationId")

      val result = controller.pollSubmission(encodedPollUrl, correlationId)(req)

      status(result) mustBe OK
      val js = contentAsJson(result)
      (js \ "status").as[String] mustBe "FATAL_ERROR"
      verifyNoInteractions(service)
    }

    "returns 200 with FATAL_ERROR when HMRC-CIS-ORG enrolment is missing" in {
      val service = mock[SubmissionService]
      val controller = mkController(service, auth = noEnrolmentReferenceAuthAction)

      val oldTimestamp = Instant.now().minusSeconds(30)
      val encodedPollUrl = java.net.URLEncoder.encode(s"http://example.com/poll?timestamp=$oldTimestamp", "UTF-8")
      val correlationId = "CORR-NO-ENROL"

      val req = FakeRequest(GET, s"/cis/submissions/poll?pollUrl=$encodedPollUrl&correlationId=$correlationId")

      val result = controller.pollSubmission(encodedPollUrl, correlationId)(req)

      status(result) mustBe OK
      val js = contentAsJson(result)
      (js \ "status").as[String] mustBe "FATAL_ERROR"
      verifyNoInteractions(service)
    }

    "returns 401 when unauthorised" in {
      val service = mock[SubmissionService]
      val controller = mkController(service, auth = rejectingAuthAction)

      val timestamp = Instant.now().minusSeconds(30)
      val encodedPollUrl = java.net.URLEncoder.encode(s"http://example.com/poll?timestamp=$timestamp", "UTF-8")
      val correlationId = "CORR-UNAUTH"

      val req = FakeRequest(GET, s"/cis/submissions/poll?pollUrl=$encodedPollUrl&correlationId=$correlationId")

      val result = controller.pollSubmission(encodedPollUrl, correlationId)(req)

      status(result) mustBe UNAUTHORIZED
      verifyNoInteractions(service)
    }
  }

  "createMonthlyNilReturnRequestJson" - {

    trait TestableMonthlyNilReturnService {
      def convertXmlToJson(xml: String): XmlConversionResult
      def createMonthlyNilReturnRequestJson(payload: BuiltSubmissionPayload): JsValue
    }

    "return correct JSON for successful conversion" in {
      val service: TestableMonthlyNilReturnService = new TestableMonthlyNilReturnService {
        def convertXmlToJson(xml: String): XmlConversionResult =
          XmlConversionResult(true, Some(Json.obj("ok" -> true)), None)

        def createMonthlyNilReturnRequestJson(payload: BuiltSubmissionPayload): JsValue = {
          convertXmlToJson(payload.envelope.toString) match {
            case XmlConversionResult(true, Some(json), _) => json
            case XmlConversionResult(false, _, Some(error)) => Json.obj("error" -> error)
            case _ => Json.obj("error" -> "unexpected conversion failure")
          }
        }
      }
      val payload = BuiltSubmissionPayload(<xml></xml>, "corr-1", "irmark-1")
      val result = service.createMonthlyNilReturnRequestJson(payload)
      result mustBe Json.obj("ok" -> true)
    }
    "return error JSON when XML conversion fails" in {
      val service: TestableMonthlyNilReturnService = new TestableMonthlyNilReturnService {
        def convertXmlToJson(xml: String): XmlConversionResult =
          XmlConversionResult(false, None, Some("Invalid XML"))

        def createMonthlyNilReturnRequestJson(payload: BuiltSubmissionPayload): JsValue = {
          convertXmlToJson(payload.envelope.toString) match {
            case XmlConversionResult(true, Some(json), _) => json
            case XmlConversionResult(false, _, Some(error)) => Json.obj("error" -> error)
            case _ => Json.obj("error" -> "unexpected conversion failure")
          }
        }
      }
      val payload = BuiltSubmissionPayload(<xml>bad</xml>, "corr-2", "irmark-2")
      val result = service.createMonthlyNilReturnRequestJson(payload)
      result mustBe Json.obj("error" -> "Invalid XML")
    }
    "return unexpected conversion failure when neither json nor error provided" in {
      val service: TestableMonthlyNilReturnService = new TestableMonthlyNilReturnService {
        def convertXmlToJson(xml: String): XmlConversionResult =
          XmlConversionResult(false, None, None)

        def createMonthlyNilReturnRequestJson(payload: BuiltSubmissionPayload): JsValue = {
          convertXmlToJson(payload.envelope.toString) match {
            case XmlConversionResult(true, Some(json), _) => json
            case XmlConversionResult(false, _, Some(error)) => Json.obj("error" -> error)
            case _ => Json.obj("error" -> "unexpected conversion failure")
          }
        }
      }
      val payload = BuiltSubmissionPayload(<xml>weird</xml>, "corr-3", "irmark-3")
      val result = service.createMonthlyNilReturnRequestJson(payload)
      result mustBe Json.obj("error" -> "unexpected conversion failure")
    }
  }

  "createMonthlyNilReturnResponseJson" - {

    trait TestableMonthlyNilResponseService {
      def convertXmlToJson(xml: String): XmlConversionResult
      def createMonthlyNilReturnResponseJson(res: SubmissionResult): JsValue
    }

    val govTalkMeta = GovTalkMeta(
      qualifier = "response",
      function = "submit",
      className = "CIS300MR",
      correlationId = "correlationId",
      gatewayTimestamp = Some("gatewayTimestamp"),
      responseEndPoint = ResponseEndPoint("/poll", 100),
      error = None
    )
    val res = SubmissionResult(SUBMITTED, "rawXml", govTalkMeta)

    "return correct JSON for successful conversion" in {
      val service: TestableMonthlyNilResponseService = new TestableMonthlyNilResponseService {
        def convertXmlToJson(xml: String): XmlConversionResult =
          XmlConversionResult(true, Some(Json.obj("ok" -> true)), None)

        def createMonthlyNilReturnResponseJson(res: SubmissionResult): JsValue = {
          convertXmlToJson(res.rawXml) match {
            case XmlConversionResult(true, Some(json), _) => json
            case _ => Json.toJson(res.rawXml)
          }
        }
      }
      val result = service.createMonthlyNilReturnResponseJson(res)
      result mustBe Json.obj("ok" -> true)
    }
    "return error JSON when XML conversion fails" in {
      val service: TestableMonthlyNilResponseService = new TestableMonthlyNilResponseService {
        def convertXmlToJson(xml: String): XmlConversionResult =
          XmlConversionResult(false, None, Some("Invalid XML"))

        def createMonthlyNilReturnResponseJson(res: SubmissionResult): JsValue = {
          convertXmlToJson(res.rawXml) match {
            case XmlConversionResult(true, Some(json), _) => json
            case _ => Json.toJson(res.rawXml)
          }
        }
      }

      val result = service.createMonthlyNilReturnResponseJson(res)
      result mustBe Json.toJson(res.rawXml)
    }
  }


  private def mkMeta(
                      corrId: String = "CID123",
                      pollSecs: Int = 15,
                      ts: Option[String] = None,
                      err: Option[GovTalkError] = None
                    ): GovTalkMeta =
    GovTalkMeta(
      qualifier = "response",
      function = "submit",
      className = "CIS300MR",
      correlationId = corrId,
      gatewayTimestamp = ts,
      responseEndPoint = ResponseEndPoint("/poll", pollSecs),
      error = err
    )

  private def mkSubmissionResult(
                                  status: SubmissionStatus,
                                  err: Option[GovTalkError] = None
                                ): SubmissionResult =
    SubmissionResult(
      status = status,
      rawXml = "<ack/>",
      meta = mkMeta(err = err)
    )

}
