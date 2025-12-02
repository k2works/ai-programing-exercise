package com.example.accounting.infrastructure.in.http

import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport.*
import com.example.accounting.application.*
import com.example.accounting.application.port.in.FinancialAnalysisUseCase
import com.example.accounting.infrastructure.in.http.dto.*
import com.example.accounting.infrastructure.in.http.JsonFormats.*

/**
 * 財務分析 API Routes
 */
class FinancialAnalysisRoutes(service: FinancialAnalysisUseCase):

  import JsonFormats.{
    financialAnalysisResponseFormat,
    financialComparisonResponseFormat,
    errorResponseFormat,
    given,
  }

  val routes: Route =
    pathPrefix("api" / "v1" / "financial-analysis") {
      concat(
        // GET /api/v1/financial-analysis/compare?fromYear=...&toYear=...
        path("compare") {
          get {
            parameters("fromYear".as[Int], "toYear".as[Int]) { (fromYear, toYear) =>
              service.compareByFiscalYears(fromYear, toYear) match
                case Right(result) =>
                  complete(FinancialComparisonResponse.fromDomain(result))
                case Left(error: NotFoundError) =>
                  complete(StatusCodes.NotFound -> ErrorResponse.fromAppError(error))
                case Left(error: ValidationError) =>
                  complete(StatusCodes.BadRequest -> ErrorResponse.fromAppError(error))
                case Left(error) =>
                  complete(StatusCodes.InternalServerError -> ErrorResponse.fromAppError(error))
            }
          }
        },
        // GET /api/v1/financial-analysis/:fiscalYear
        path(IntNumber) { fiscalYear =>
          get {
            service.analyzeByFiscalYear(fiscalYear) match
              case Right(result) =>
                complete(FinancialAnalysisResponse.fromDomain(result))
              case Left(error: NotFoundError) =>
                complete(StatusCodes.NotFound -> ErrorResponse.fromAppError(error))
              case Left(error: ValidationError) =>
                complete(StatusCodes.BadRequest -> ErrorResponse.fromAppError(error))
              case Left(error) =>
                complete(StatusCodes.InternalServerError -> ErrorResponse.fromAppError(error))
          }
        },
      )
    }
