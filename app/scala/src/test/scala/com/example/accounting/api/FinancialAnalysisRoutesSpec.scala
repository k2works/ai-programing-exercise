package com.example.accounting.api

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport.*
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.testkit.ScalatestRouteTest
import com.example.accounting.application.service.FinancialAnalysisService
import com.example.accounting.infrastructure.in.http.FinancialAnalysisRoutes
import com.example.accounting.infrastructure.in.http.dto.*
import com.example.accounting.infrastructure.in.http.JsonFormats.{*, given}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * 財務分析 API Routes のテスト
 */
class FinancialAnalysisRoutesSpec extends AnyFlatSpec with Matchers with ScalatestRouteTest:

  private val service = FinancialAnalysisService()
  private val routes = FinancialAnalysisRoutes(service)

  behavior of "GET /api/v1/financial-analysis/:fiscalYear"

  it should "令和3年度の財務分析を取得できる" in {
    Get("/api/v1/financial-analysis/2021") ~> routes.routes ~> check {
      status shouldBe StatusCodes.OK
      val response = responseAs[FinancialAnalysisResponse]
      response.fiscalYear shouldBe 2021

      // 収益性指標
      response.profitability.grossProfitMargin shouldBe BigDecimal("62.29") +- BigDecimal("0.01")
      response.profitability.operatingProfitMargin shouldBe BigDecimal("17.00") +- BigDecimal("0.01")
      response.profitability.ordinaryProfitMargin shouldBe BigDecimal("16.96") +- BigDecimal("0.01")
      response.profitability.sellingExpenseRatio shouldBe BigDecimal("45.29") +- BigDecimal("0.01")

      // 効率性指標
      response.efficiency.totalAssetTurnover shouldBe BigDecimal("2.02") +- BigDecimal("0.01")
      response.efficiency.fixedAssetTurnover shouldBe BigDecimal("30.99") +- BigDecimal("0.01")

      // 安全性指標
      response.safety.currentRatio shouldBe BigDecimal("314.33") +- BigDecimal("0.01")
      response.safety.equityRatio shouldBe BigDecimal("69.48") +- BigDecimal("0.01")
      response.safety.debtRatio shouldBe BigDecimal("43.93") +- BigDecimal("0.01")
    }
  }

  it should "令和4年度の財務分析を取得できる" in {
    Get("/api/v1/financial-analysis/2022") ~> routes.routes ~> check {
      status shouldBe StatusCodes.OK
      val response = responseAs[FinancialAnalysisResponse]
      response.fiscalYear shouldBe 2022

      // 収益性指標
      response.profitability.operatingProfitMargin shouldBe BigDecimal("11.59") +- BigDecimal("0.01")
    }
  }

  it should "存在しない年度は 404 を返す" in {
    Get("/api/v1/financial-analysis/2020") ~> routes.routes ~> check {
      status shouldBe StatusCodes.NotFound
      val error = responseAs[ErrorResponse]
      error.code shouldBe "NOT_FOUND"
    }
  }

  behavior of "GET /api/v1/financial-analysis/compare"

  it should "複数年度の比較分析を取得できる" in {
    Get("/api/v1/financial-analysis/compare?fromYear=2021&toYear=2022") ~> routes.routes ~> check {
      status shouldBe StatusCodes.OK
      val response = responseAs[FinancialComparisonResponse]

      response.results should have size 2
      response.results(0).fiscalYear shouldBe 2021
      response.results(1).fiscalYear shouldBe 2022

      response.changes shouldBe defined
      response.changes.foreach { changes =>
        // 営業利益率の変化
        changes.profitability.operatingProfitMarginChange shouldBe BigDecimal("-5.41") +- BigDecimal("0.1")
      }
    }
  }

  it should "開始年度が終了年度より後の場合は 400 を返す" in {
    Get("/api/v1/financial-analysis/compare?fromYear=2022&toYear=2021") ~> routes.routes ~> check {
      status shouldBe StatusCodes.BadRequest
      val error = responseAs[ErrorResponse]
      error.code shouldBe "VALIDATION_ERROR"
    }
  }

  it should "存在しない年度を含む場合は 404 を返す" in {
    Get("/api/v1/financial-analysis/compare?fromYear=2020&toYear=2022") ~> routes.routes ~> check {
      status shouldBe StatusCodes.NotFound
      val error = responseAs[ErrorResponse]
      error.code shouldBe "NOT_FOUND"
    }
  }
