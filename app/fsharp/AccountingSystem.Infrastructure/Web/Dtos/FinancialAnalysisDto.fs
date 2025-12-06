namespace AccountingSystem.Infrastructure.Web.Dtos

open AccountingSystem.Domain.Models
open AccountingSystem.Application.Port.In

/// <summary>
/// 財務データレスポンス DTO
/// </summary>
type FinancialDataResponse = {
    FiscalYear: int
    AsOfDate: string

    // 資産の部
    CurrentAssets: decimal
    QuickAssets: decimal
    Inventory: decimal
    FixedAssets: decimal
    TangibleFixedAssets: decimal
    TotalAssets: decimal

    // 負債の部
    CurrentLiabilities: decimal
    FixedLiabilities: decimal
    TotalLiabilities: decimal

    // 純資産の部
    Equity: decimal

    // 損益の部
    NetSales: decimal
    CostOfSales: decimal
    GrossProfit: decimal
    SellingGeneralAdminExpenses: decimal
    OperatingIncome: decimal
    OrdinaryIncome: decimal
    NetIncome: decimal
}

module FinancialDataResponse =
    let from (data: FinancialData) : FinancialDataResponse = {
        FiscalYear = data.FiscalYear
        AsOfDate = data.AsOfDate.ToString("yyyy-MM-dd")
        CurrentAssets = data.CurrentAssets
        QuickAssets = data.QuickAssets
        Inventory = data.Inventory
        FixedAssets = data.FixedAssets
        TangibleFixedAssets = data.TangibleFixedAssets
        TotalAssets = data.TotalAssets
        CurrentLiabilities = data.CurrentLiabilities
        FixedLiabilities = data.FixedLiabilities
        TotalLiabilities = data.TotalLiabilities
        Equity = data.Equity
        NetSales = data.NetSales
        CostOfSales = data.CostOfSales
        GrossProfit = data.GrossProfit
        SellingGeneralAdminExpenses = data.SellingGeneralAdminExpenses
        OperatingIncome = data.OperatingIncome
        OrdinaryIncome = data.OrdinaryIncome
        NetIncome = data.NetIncome
    }

/// <summary>
/// 包括的財務指標レスポンス DTO
/// </summary>
type ComprehensiveFinancialRatiosResponse = {
    FiscalYear: int

    // 収益性指標
    GrossProfitMargin: decimal
    OperatingProfitMargin: decimal
    OrdinaryProfitMargin: decimal
    SellingExpenseRatio: decimal
    Roa: decimal
    Roe: decimal

    // 効率性指標
    TotalAssetTurnover: decimal
    AccountsReceivableTurnover: decimal
    InventoryTurnover: decimal
    TangibleFixedAssetTurnover: decimal

    // 安全性指標
    CurrentRatio: decimal
    QuickRatio: decimal
    FixedRatio: decimal
    FixedLongTermSuitabilityRatio: decimal
    DebtRatio: decimal
    EquityRatio: decimal
}

module ComprehensiveFinancialRatiosResponse =
    let from (ratios: ComprehensiveFinancialRatios) : ComprehensiveFinancialRatiosResponse = {
        FiscalYear = ratios.FiscalYear
        GrossProfitMargin = ratios.GrossProfitMargin
        OperatingProfitMargin = ratios.OperatingProfitMargin
        OrdinaryProfitMargin = ratios.OrdinaryProfitMargin
        SellingExpenseRatio = ratios.SellingExpenseRatio
        Roa = ratios.Roa
        Roe = ratios.Roe
        TotalAssetTurnover = ratios.TotalAssetTurnover
        AccountsReceivableTurnover = ratios.AccountsReceivableTurnover
        InventoryTurnover = ratios.InventoryTurnover
        TangibleFixedAssetTurnover = ratios.TangibleFixedAssetTurnover
        CurrentRatio = ratios.CurrentRatio
        QuickRatio = ratios.QuickRatio
        FixedRatio = ratios.FixedRatio
        FixedLongTermSuitabilityRatio = ratios.FixedLongTermSuitabilityRatio
        DebtRatio = ratios.DebtRatio
        EquityRatio = ratios.EquityRatio
    }

/// <summary>
/// 収益性分析レスポンス DTO
/// </summary>
type ProfitabilityAnalysisResponse = {
    FiscalYear: int
    GrossProfitMargin: decimal
    OperatingProfitMargin: decimal
    OrdinaryProfitMargin: decimal
    SellingExpenseRatio: decimal
    Roa: decimal
    Roe: decimal
    Analysis: string
}

module ProfitabilityAnalysisResponse =
    let from (analysis: ProfitabilityAnalysis) : ProfitabilityAnalysisResponse = {
        FiscalYear = analysis.FiscalYear
        GrossProfitMargin = analysis.GrossProfitMargin
        OperatingProfitMargin = analysis.OperatingProfitMargin
        OrdinaryProfitMargin = analysis.OrdinaryProfitMargin
        SellingExpenseRatio = analysis.SellingExpenseRatio
        Roa = analysis.Roa
        Roe = analysis.Roe
        Analysis = analysis.Analysis
    }

/// <summary>
/// 効率性分析レスポンス DTO
/// </summary>
type EfficiencyAnalysisResponse = {
    FiscalYear: int
    TotalAssetTurnover: decimal
    AccountsReceivableTurnover: decimal
    InventoryTurnover: decimal
    TangibleFixedAssetTurnover: decimal
    Analysis: string
}

module EfficiencyAnalysisResponse =
    let from (analysis: EfficiencyAnalysis) : EfficiencyAnalysisResponse = {
        FiscalYear = analysis.FiscalYear
        TotalAssetTurnover = analysis.TotalAssetTurnover
        AccountsReceivableTurnover = analysis.AccountsReceivableTurnover
        InventoryTurnover = analysis.InventoryTurnover
        TangibleFixedAssetTurnover = analysis.TangibleFixedAssetTurnover
        Analysis = analysis.Analysis
    }

/// <summary>
/// 安全性分析レスポンス DTO
/// </summary>
type SafetyAnalysisResponse = {
    FiscalYear: int
    CurrentRatio: decimal
    QuickRatio: decimal
    FixedRatio: decimal
    FixedLongTermSuitabilityRatio: decimal
    DebtRatio: decimal
    EquityRatio: decimal
    Analysis: string
}

module SafetyAnalysisResponse =
    let from (analysis: SafetyAnalysis) : SafetyAnalysisResponse = {
        FiscalYear = analysis.FiscalYear
        CurrentRatio = analysis.CurrentRatio
        QuickRatio = analysis.QuickRatio
        FixedRatio = analysis.FixedRatio
        FixedLongTermSuitabilityRatio = analysis.FixedLongTermSuitabilityRatio
        DebtRatio = analysis.DebtRatio
        EquityRatio = analysis.EquityRatio
        Analysis = analysis.Analysis
    }

/// <summary>
/// 年度比較リクエスト DTO
/// </summary>
type CompareRatiosRequest = {
    FiscalYears: int array
}
