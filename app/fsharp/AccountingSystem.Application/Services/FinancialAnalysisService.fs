namespace AccountingSystem.Application.Services

open System
open System.Threading.Tasks
open AccountingSystem.Domain.Models
open AccountingSystem.Application.Port.In
open AccountingSystem.Application.Port.Out

/// <summary>
/// 財務分析アプリケーションサービス
/// D社の事例に基づく包括的な財務分析機能を提供
/// </summary>
type FinancialAnalysisService(repository: IFinancialStatementRepository) =

    /// <summary>
    /// 会計年度から期間を計算（4月〜3月の日本の会計年度）
    /// </summary>
    let getFiscalYearDates (fiscalYear: int) =
        let startDate = DateTime(fiscalYear, 4, 1)
        let endDate = DateTime(fiscalYear + 1, 3, 31)
        (startDate, endDate)

    /// <summary>
    /// 収益性の評価コメントを生成
    /// </summary>
    let analyzeProfitabilityComment (ratios: ComprehensiveFinancialRatios) =
        let comments = ResizeArray<string>()

        if ratios.GrossProfitMargin >= 50m then
            comments.Add($"売上高総利益率 {ratios.GrossProfitMargin}%% は高水準です。付加価値の高い製品・サービスを提供しています。")
        elif ratios.GrossProfitMargin >= 30m then
            comments.Add($"売上高総利益率 {ratios.GrossProfitMargin}%% は標準的な水準です。")
        else
            comments.Add($"売上高総利益率 {ratios.GrossProfitMargin}%% は低めです。原価管理の改善が必要かもしれません。")

        if ratios.OperatingProfitMargin >= 10m then
            comments.Add($"売上高営業利益率 {ratios.OperatingProfitMargin}%% は良好です。本業での収益力が高いです。")
        elif ratios.OperatingProfitMargin >= 5m then
            comments.Add($"売上高営業利益率 {ratios.OperatingProfitMargin}%% は標準的です。")
        else
            comments.Add($"売上高営業利益率 {ratios.OperatingProfitMargin}%% は改善の余地があります。販管費の見直しを検討してください。")

        if ratios.Roe >= 15m then
            comments.Add($"ROE {ratios.Roe}%% は優良企業の水準です。")
        elif ratios.Roe >= 8m then
            comments.Add($"ROE {ratios.Roe}%% は標準的な水準です。")
        else
            comments.Add($"ROE {ratios.Roe}%% は資本効率の改善が求められます。")

        String.Join(" ", comments)

    /// <summary>
    /// 効率性の評価コメントを生成
    /// </summary>
    let analyzeEfficiencyComment (ratios: ComprehensiveFinancialRatios) =
        let comments = ResizeArray<string>()

        if ratios.TotalAssetTurnover >= 1.5m then
            comments.Add($"総資本回転率 {ratios.TotalAssetTurnover}回 は良好です。資産を効率的に活用しています。")
        elif ratios.TotalAssetTurnover >= 1.0m then
            comments.Add($"総資本回転率 {ratios.TotalAssetTurnover}回 は標準的です。")
        else
            comments.Add($"総資本回転率 {ratios.TotalAssetTurnover}回 は資産効率の改善余地があります。")

        if ratios.InventoryTurnover >= 6m then
            comments.Add($"棚卸資産回転率 {ratios.InventoryTurnover}回 は良好です。在庫管理が効率的です。")
        elif ratios.InventoryTurnover >= 4m then
            comments.Add($"棚卸資産回転率 {ratios.InventoryTurnover}回 は標準的です。")
        else
            comments.Add($"棚卸資産回転率 {ratios.InventoryTurnover}回 は在庫の滞留が懸念されます。")

        String.Join(" ", comments)

    /// <summary>
    /// 安全性の評価コメントを生成
    /// </summary>
    let analyzeSafetyComment (ratios: ComprehensiveFinancialRatios) =
        let comments = ResizeArray<string>()

        if ratios.CurrentRatio >= 200m then
            comments.Add($"流動比率 {ratios.CurrentRatio}%% は非常に良好です。短期的な支払能力に問題ありません。")
        elif ratios.CurrentRatio >= 120m then
            comments.Add($"流動比率 {ratios.CurrentRatio}%% は標準的です。")
        else
            comments.Add($"流動比率 {ratios.CurrentRatio}%% は短期的な資金繰りに注意が必要です。")

        if ratios.EquityRatio >= 50m then
            comments.Add($"自己資本比率 {ratios.EquityRatio}%% は健全な財務体質です。")
        elif ratios.EquityRatio >= 30m then
            comments.Add($"自己資本比率 {ratios.EquityRatio}%% は標準的です。")
        else
            comments.Add($"自己資本比率 {ratios.EquityRatio}%% は財務体質の改善が望まれます。")

        if ratios.FixedRatio <= 100m then
            comments.Add($"固定比率 {ratios.FixedRatio}%% は良好です。固定資産を自己資本で賄えています。")
        else
            comments.Add($"固定比率 {ratios.FixedRatio}%% は固定資産の一部を他人資本で調達しています。")

        String.Join(" ", comments)

    interface IFinancialAnalysisUseCase with

        /// <summary>
        /// 指定した会計年度の財務データを取得する
        /// </summary>
        member _.GetFinancialDataAsync(fiscalYear: int) : Task<Result<FinancialData, string>> =
            task {
                try
                    let (startDate, endDate) = getFiscalYearDates fiscalYear
                    let! balanceSheet = repository.GenerateBalanceSheetAsync(endDate)
                    let! incomeStatement = repository.GenerateIncomeStatementAsync(startDate, endDate)

                    if balanceSheet.TotalAssets = 0m && incomeStatement.TotalRevenues = 0m then
                        return Error $"会計年度 {fiscalYear} のデータが見つかりません"
                    else
                        let financialData = FinancialData.fromStatements balanceSheet incomeStatement
                        return Ok financialData
                with ex ->
                    return Error $"財務データの取得に失敗しました: {ex.Message}"
            }

        /// <summary>
        /// 指定した会計年度の包括的な財務指標を計算する
        /// </summary>
        member this.CalculateComprehensiveRatiosAsync(fiscalYear: int) : Task<Result<ComprehensiveFinancialRatios, string>> =
            task {
                let useCase = this :> IFinancialAnalysisUseCase
                let! dataResult = useCase.GetFinancialDataAsync(fiscalYear)
                match dataResult with
                | Ok data ->
                    let ratios = ComprehensiveFinancialRatios.calculate data
                    return Ok ratios
                | Error msg ->
                    return Error msg
            }

        /// <summary>
        /// 複数年度の財務指標を比較する
        /// </summary>
        member this.CompareFinancialRatiosAsync(fiscalYears: int list) : Task<Result<ComprehensiveFinancialRatios list, string>> =
            task {
                let useCase = this :> IFinancialAnalysisUseCase
                let mutable results = []
                let mutable errorMsg = None

                for year in fiscalYears do
                    if errorMsg.IsNone then
                        let! result = useCase.CalculateComprehensiveRatiosAsync(year)
                        match result with
                        | Ok ratios -> results <- results @ [ratios]
                        | Error msg -> errorMsg <- Some msg

                match errorMsg with
                | Some msg -> return Error msg
                | None -> return Ok results
            }

        /// <summary>
        /// 収益性分析を実行する
        /// </summary>
        member this.AnalyzeProfitabilityAsync(fiscalYear: int) : Task<Result<ProfitabilityAnalysis, string>> =
            task {
                let useCase = this :> IFinancialAnalysisUseCase
                let! ratiosResult = useCase.CalculateComprehensiveRatiosAsync(fiscalYear)
                match ratiosResult with
                | Ok ratios ->
                    let analysis = {
                        FiscalYear = fiscalYear
                        GrossProfitMargin = ratios.GrossProfitMargin
                        OperatingProfitMargin = ratios.OperatingProfitMargin
                        OrdinaryProfitMargin = ratios.OrdinaryProfitMargin
                        SellingExpenseRatio = ratios.SellingExpenseRatio
                        Roa = ratios.Roa
                        Roe = ratios.Roe
                        Analysis = analyzeProfitabilityComment ratios
                    }
                    return Ok analysis
                | Error msg ->
                    return Error msg
            }

        /// <summary>
        /// 効率性分析を実行する
        /// </summary>
        member this.AnalyzeEfficiencyAsync(fiscalYear: int) : Task<Result<EfficiencyAnalysis, string>> =
            task {
                let useCase = this :> IFinancialAnalysisUseCase
                let! ratiosResult = useCase.CalculateComprehensiveRatiosAsync(fiscalYear)
                match ratiosResult with
                | Ok ratios ->
                    let analysis = {
                        FiscalYear = fiscalYear
                        TotalAssetTurnover = ratios.TotalAssetTurnover
                        AccountsReceivableTurnover = ratios.AccountsReceivableTurnover
                        InventoryTurnover = ratios.InventoryTurnover
                        TangibleFixedAssetTurnover = ratios.TangibleFixedAssetTurnover
                        Analysis = analyzeEfficiencyComment ratios
                    }
                    return Ok analysis
                | Error msg ->
                    return Error msg
            }

        /// <summary>
        /// 安全性分析を実行する
        /// </summary>
        member this.AnalyzeSafetyAsync(fiscalYear: int) : Task<Result<SafetyAnalysis, string>> =
            task {
                let useCase = this :> IFinancialAnalysisUseCase
                let! ratiosResult = useCase.CalculateComprehensiveRatiosAsync(fiscalYear)
                match ratiosResult with
                | Ok ratios ->
                    let analysis = {
                        FiscalYear = fiscalYear
                        CurrentRatio = ratios.CurrentRatio
                        QuickRatio = ratios.QuickRatio
                        FixedRatio = ratios.FixedRatio
                        FixedLongTermSuitabilityRatio = ratios.FixedLongTermSuitabilityRatio
                        DebtRatio = ratios.DebtRatio
                        EquityRatio = ratios.EquityRatio
                        Analysis = analyzeSafetyComment ratios
                    }
                    return Ok analysis
                | Error msg ->
                    return Error msg
            }
