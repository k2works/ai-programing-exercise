namespace ManagementAccounting.Infrastructure.Web.Dtos

open System
open ManagementAccounting.Domain.Models
open ManagementAccounting.Application.Ports.In

/// <summary>
/// 財務分析リクエスト DTO
/// </summary>
[<CLIMutable>]
type AnalyzeFinancialDataRequestDto = {
    FiscalYear: int
    UseCache: bool
}

/// <summary>
/// 財務データ レスポンス DTO
/// </summary>
[<CLIMutable>]
type FinancialDataResponseDto = {
    FiscalYear: int
    Sales: decimal
    OperatingProfit: decimal
    TotalAssets: decimal
    TangibleFixedAssets: decimal
    CurrentAssets: decimal
    CurrentLiabilities: decimal
    QuickAssets: decimal
    Equity: decimal
}

/// <summary>
/// 財務比率 レスポンス DTO
/// </summary>
[<CLIMutable>]
type FinancialRatiosResponseDto = {
    OperatingProfitMargin: decimal
    TotalAssetTurnover: decimal
    FixedAssetTurnover: decimal
    CurrentRatio: decimal
    QuickRatio: decimal
    EquityRatio: decimal
    ReturnOnAssets: decimal
    ReturnOnEquity: decimal
}

/// <summary>
/// 財務分析結果 レスポンス DTO
/// </summary>
[<CLIMutable>]
type FinancialAnalysisResultResponseDto = {
    FiscalYear: int
    Data: FinancialDataResponseDto
    Ratios: FinancialRatiosResponseDto
    AnalyzedAt: DateTime
}

/// <summary>
/// DTO 変換モジュール
/// </summary>
module FinancialAnalysisDto =

    let toRequest (dto: AnalyzeFinancialDataRequestDto) : AnalyzeFinancialDataRequest =
        {
            FiscalYear = dto.FiscalYear
            UseCache = dto.UseCache
        }

    let toDataResponse (data: FinancialData) : FinancialDataResponseDto =
        {
            FiscalYear = data.FiscalYear
            Sales = data.Sales
            OperatingProfit = data.OperatingProfit
            TotalAssets = data.TotalAssets
            TangibleFixedAssets = data.TangibleFixedAssets
            CurrentAssets = data.CurrentAssets
            CurrentLiabilities = data.CurrentLiabilities
            QuickAssets = data.QuickAssets
            Equity = data.Equity
        }

    let toRatiosResponse (ratios: FinancialRatios) : FinancialRatiosResponseDto =
        {
            OperatingProfitMargin = Math.Round(ratios.OperatingProfitMargin * 100m, 2)
            TotalAssetTurnover = Math.Round(ratios.TotalAssetTurnover, 2)
            FixedAssetTurnover = Math.Round(ratios.FixedAssetTurnover, 2)
            CurrentRatio = Math.Round(ratios.CurrentRatio * 100m, 2)
            QuickRatio = Math.Round(ratios.QuickRatio * 100m, 2)
            EquityRatio = Math.Round(ratios.EquityRatio * 100m, 2)
            ReturnOnAssets = Math.Round(ratios.ReturnOnAssets * 100m, 2)
            ReturnOnEquity = Math.Round(ratios.ReturnOnEquity * 100m, 2)
        }

    let toResultResponse (result: FinancialAnalysisResult) : FinancialAnalysisResultResponseDto =
        {
            FiscalYear = result.FiscalYear
            Data = toDataResponse result.Data
            Ratios = toRatiosResponse result.Ratios
            AnalyzedAt = result.AnalyzedAt
        }
