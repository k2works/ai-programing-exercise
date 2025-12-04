namespace AccountingSystem.Application.Services

open System
open System.Threading.Tasks
open AccountingSystem.Domain.Models
open AccountingSystem.Application.Port.In
open AccountingSystem.Application.Port.Out

/// <summary>
/// 財務諸表アプリケーションサービス
/// 貸借対照表・損益計算書の生成と財務指標の計算を行う
/// Infrastructure への依存は IFinancialStatementRepository を通じて抽象化
/// </summary>
type FinancialStatementService(repository: IFinancialStatementRepository) =

    interface IFinancialStatementUseCase with

        /// <summary>
        /// 貸借対照表を生成する
        /// </summary>
        /// <param name="asOfDate">基準日</param>
        /// <returns>貸借対照表</returns>
        member _.GenerateBalanceSheetAsync(asOfDate: DateTime) : Task<BalanceSheet> =
            repository.GenerateBalanceSheetAsync(asOfDate)

        /// <summary>
        /// 損益計算書を生成する
        /// </summary>
        /// <param name="fromDate">開始日</param>
        /// <param name="toDate">終了日</param>
        /// <returns>損益計算書</returns>
        member _.GenerateIncomeStatementAsync(fromDate: DateTime, toDate: DateTime) : Task<IncomeStatement> =
            repository.GenerateIncomeStatementAsync(fromDate, toDate)

        /// <summary>
        /// 財務指標を計算する
        /// </summary>
        /// <param name="asOfDate">貸借対照表の基準日</param>
        /// <param name="fromDate">損益計算書の開始日</param>
        /// <param name="toDate">損益計算書の終了日</param>
        /// <returns>財務指標</returns>
        member _.CalculateFinancialRatiosAsync(asOfDate: DateTime, fromDate: DateTime, toDate: DateTime) : Task<FinancialRatios> =
            task {
                let! balanceSheet = repository.GenerateBalanceSheetAsync(asOfDate)
                let! incomeStatement = repository.GenerateIncomeStatementAsync(fromDate, toDate)
                return FinancialRatios.calculate balanceSheet incomeStatement
            }
