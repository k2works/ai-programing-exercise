package com.example.accounting.infrastructure.http

import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport.*
import spray.json.*
import com.example.accounting.application.FinancialStatementService
import com.example.accounting.infrastructure.http.dto.*
import com.example.accounting.infrastructure.http.JsonFormats.{*, given}
import scalikejdbc.*

import java.time.LocalDate
import java.time.format.DateTimeParseException

/**
 * 財務諸表 API Routes
 */
class FinancialStatementRoutes(service: FinancialStatementService):

  val routes: Route =
    pathPrefix("api" / "financial-statements") {
      concat(
        // GET /api/financial-statements/balance-sheet?asOfDate=...
        path("balance-sheet") {
          get {
            parameter("asOfDate".as[String]) { asOfDateStr =>
              try
                val asOfDate = LocalDate.parse(asOfDateStr)
                DB.readOnly { implicit session =>
                  val balanceSheet = service.generateBalanceSheet(asOfDate)
                  complete(BalanceSheetResponse.fromDomain(balanceSheet))
                }
              catch
                case _: DateTimeParseException =>
                  complete(
                    StatusCodes.BadRequest -> ErrorResponse(
                      code = "VALIDATION_ERROR",
                      message = "日付の形式が不正です（YYYY-MM-DD）",
                    )
                  )
                case e: Exception =>
                  complete(
                    StatusCodes.InternalServerError -> ErrorResponse(
                      code = "INTERNAL_ERROR",
                      message = "貸借対照表の生成に失敗しました",
                      details = Some(List(e.getMessage)),
                    )
                  )
            }
          }
        },
        // GET /api/financial-statements/income-statement?fromDate=...&toDate=...
        path("income-statement") {
          get {
            parameters("fromDate".as[String], "toDate".as[String]) { (fromDateStr, toDateStr) =>
              try
                val fromDate = LocalDate.parse(fromDateStr)
                val toDate = LocalDate.parse(toDateStr)
                DB.readOnly { implicit session =>
                  val incomeStatement = service.generateIncomeStatement(fromDate, toDate)
                  complete(IncomeStatementResponse.fromDomain(incomeStatement))
                }
              catch
                case _: DateTimeParseException =>
                  complete(
                    StatusCodes.BadRequest -> ErrorResponse(
                      code = "VALIDATION_ERROR",
                      message = "日付の形式が不正です（YYYY-MM-DD）",
                    )
                  )
                case e: Exception =>
                  complete(
                    StatusCodes.InternalServerError -> ErrorResponse(
                      code = "INTERNAL_ERROR",
                      message = "損益計算書の生成に失敗しました",
                      details = Some(List(e.getMessage)),
                    )
                  )
            }
          }
        },
        // GET /api/financial-statements/ratios?asOfDate=...&fromDate=...&toDate=...
        path("ratios") {
          get {
            parameters("asOfDate".as[String], "fromDate".as[String], "toDate".as[String]) {
              (asOfDateStr, fromDateStr, toDateStr) =>
                try
                  val asOfDate = LocalDate.parse(asOfDateStr)
                  val fromDate = LocalDate.parse(fromDateStr)
                  val toDate = LocalDate.parse(toDateStr)
                  DB.readOnly { implicit session =>
                    val balanceSheet = service.generateBalanceSheet(asOfDate)
                    val incomeStatement = service.generateIncomeStatement(fromDate, toDate)
                    val ratios = service.calculateFinancialRatios(balanceSheet, incomeStatement)
                    complete(FinancialRatiosResponse.fromDomain(ratios))
                  }
                catch
                  case _: DateTimeParseException =>
                    complete(
                      StatusCodes.BadRequest -> ErrorResponse(
                        code = "VALIDATION_ERROR",
                        message = "日付の形式が不正です（YYYY-MM-DD）",
                      )
                    )
                  case e: Exception =>
                    complete(
                      StatusCodes.InternalServerError -> ErrorResponse(
                        code = "INTERNAL_ERROR",
                        message = "財務指標の計算に失敗しました",
                        details = Some(List(e.getMessage)),
                      )
                    )
            }
          }
        },
      )
    }
