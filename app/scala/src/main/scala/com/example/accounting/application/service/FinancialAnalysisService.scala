package com.example.accounting.application.service

import com.example.accounting.application.*
import com.example.accounting.application.port.in.FinancialAnalysisUseCase
import com.example.accounting.domain.analysis.*

/**
 * 財務分析サービス（FinancialAnalysisUseCase の実装）
 *
 * 財務分析機能を提供するアプリケーションサービス
 */
class FinancialAnalysisService extends FinancialAnalysisUseCase:

  /**
   * 指定した会計年度の財務分析を実行
   *
   * 現在はハードコードされた D 社のデータを使用
   * 将来的にはリポジトリから財務データを取得
   */
  override def analyzeByFiscalYear(fiscalYear: Int): Either[AppError, AnalysisResult] =
    getFinancialData(fiscalYear) match
      case Left(error) => Left(error)
      case Right(data) =>
        FinancialRatioAnalyzer.analyze(data) match
          case Left(analysisError) =>
            Left(ValidationError(analysisError.message))
          case Right(result) =>
            Right(result)

  /**
   * 複数年度の財務指標を比較
   */
  override def compareByFiscalYears(fromYear: Int, toYear: Int): Either[AppError, ComparisonResult] =
    if fromYear > toYear then
      Left(ValidationError("開始年度は終了年度より前である必要があります"))
    else
      val years = (fromYear to toYear).toList
      val dataResults = years.map(getFinancialData)

      // エラーがあれば最初のエラーを返す
      val errors = dataResults.collect { case Left(e) => e }
      if errors.nonEmpty then
        Left(errors.head)
      else
        val dataList = dataResults.collect { case Right(d) => d }
        FinancialRatioAnalyzer.compare(dataList) match
          case Left(analysisError) =>
            Left(ValidationError(analysisError.message))
          case Right(result) =>
            Right(result)

  /**
   * 会計年度に応じた財務データを取得
   *
   * 現在はハードコードされたデータを返す
   * 将来的にはリポジトリから取得するように拡張可能
   */
  private def getFinancialData(fiscalYear: Int): Either[AppError, FinancialData] =
    fiscalYear match
      case 2021 => Right(FinancialData.fy2021Data)
      case 2022 => Right(FinancialData.fy2022Data)
      case _    => Left(NotFoundError(s"会計年度 $fiscalYear のデータが見つかりません"))
