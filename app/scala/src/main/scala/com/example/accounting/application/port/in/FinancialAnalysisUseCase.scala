package com.example.accounting.application.port.in

import com.example.accounting.application.AppError
import com.example.accounting.domain.analysis.{AnalysisResult, ComparisonResult}

/**
 * 財務分析ユースケース（Input Port）
 *
 * 財務分析機能のユースケースを定義するインターフェース
 */
trait FinancialAnalysisUseCase:

  /**
   * 指定した会計年度の財務分析を実行
   *
   * @param fiscalYear 会計年度
   * @return 分析結果または AppError
   */
  def analyzeByFiscalYear(fiscalYear: Int): Either[AppError, AnalysisResult]

  /**
   * 複数年度の財務指標を比較
   *
   * @param fromYear 開始年度
   * @param toYear 終了年度
   * @return 比較結果または AppError
   */
  def compareByFiscalYears(fromYear: Int, toYear: Int): Either[AppError, ComparisonResult]
