package com.example.management.application

import cats.effect.IO
import cats.implicits.*
import com.example.management.domain.*
import com.example.management.adapter.client.FinancialAccountingClient
import com.example.common.domain.*

/**
 * 財務分析サービス
 *
 * 財務会計コンテキストからデータを取得し、
 * 管理会計の観点で分析を行います。
 */
class FinancialAnalysisService(client: FinancialAccountingClient):

  /**
   * 指定した会計年度の財務分析を実行
   */
  def analyze(fiscalYear: FiscalYear): IO[Either[String, AnalysisResult]] =
    client.getFinancialData(fiscalYear).map {
      case Right(data) =>
        Right(FinancialRatioCalculator.analyze(data))
      case Left(error) =>
        Left(error)
    }

  /**
   * 複数年度の比較分析を実行
   */
  def compareYears(
      fromYear: FiscalYear,
      toYear: FiscalYear
  ): IO[Either[String, ComparisonResult]] =
    if fromYear.value > toYear.value then
      IO.pure(Left("fromYear must be less than or equal to toYear"))
    else
      client.getFinancialDataRange(fromYear, toYear).map {
        case Right(dataList) =>
          val results = dataList.map(FinancialRatioCalculator.analyze)
          val trends = TrendAnalysis.analyze(dataList)
          Right(ComparisonResult(results, trends))
        case Left(error) =>
          Left(error)
      }

  /**
   * 財務データを直接取得（デバッグ用）
   */
  def getFinancialData(fiscalYear: FiscalYear): IO[Either[String, FinancialData]] =
    client.getFinancialData(fiscalYear)
