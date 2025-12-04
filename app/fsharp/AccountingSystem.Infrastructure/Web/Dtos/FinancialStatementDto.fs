namespace AccountingSystem.Infrastructure.Web.Dtos

open System
open AccountingSystem.Domain.Models

/// <summary>
/// 貸借対照表項目レスポンス DTO
/// </summary>
[<CLIMutable>]
type BalanceSheetItemResponse = {
    AccountCode: string
    AccountName: string
    Balance: decimal
    Percentage: decimal
}

/// <summary>
/// 貸借対照表レスポンス DTO
/// </summary>
[<CLIMutable>]
type BalanceSheetResponse = {
    AsOfDate: DateTime
    Assets: BalanceSheetItemResponse list
    Liabilities: BalanceSheetItemResponse list
    Equity: BalanceSheetItemResponse list
    TotalAssets: decimal
    TotalLiabilities: decimal
    TotalEquity: decimal
    TotalLiabilitiesAndEquity: decimal
}

module BalanceSheetResponse =
    /// <summary>
    /// Domain Model からの変換
    /// </summary>
    let from (balanceSheet: BalanceSheet) : BalanceSheetResponse =
        let mapItem (item: BalanceSheetItem) : BalanceSheetItemResponse =
            {
                AccountCode = item.AccountCode
                AccountName = item.AccountName
                Balance = item.Balance
                Percentage = item.Percentage
            }

        {
            AsOfDate = balanceSheet.AsOfDate
            Assets = balanceSheet.Assets |> List.map mapItem
            Liabilities = balanceSheet.Liabilities |> List.map mapItem
            Equity = balanceSheet.Equity |> List.map mapItem
            TotalAssets = balanceSheet.TotalAssets
            TotalLiabilities = balanceSheet.TotalLiabilities
            TotalEquity = balanceSheet.TotalEquity
            TotalLiabilitiesAndEquity = balanceSheet.TotalLiabilitiesAndEquity
        }

/// <summary>
/// 損益計算書項目レスポンス DTO
/// </summary>
[<CLIMutable>]
type IncomeStatementItemResponse = {
    AccountCode: string
    AccountName: string
    Balance: decimal
    Percentage: decimal
}

/// <summary>
/// 損益計算書レスポンス DTO
/// </summary>
[<CLIMutable>]
type IncomeStatementResponse = {
    FromDate: DateTime
    ToDate: DateTime
    Revenues: IncomeStatementItemResponse list
    Expenses: IncomeStatementItemResponse list
    GrossProfit: decimal
    OperatingIncome: decimal
    NetIncome: decimal
    TotalRevenues: decimal
    TotalExpenses: decimal
}

module IncomeStatementResponse =
    /// <summary>
    /// Domain Model からの変換
    /// </summary>
    let from (incomeStatement: IncomeStatement) : IncomeStatementResponse =
        let mapItem (item: IncomeStatementItem) : IncomeStatementItemResponse =
            {
                AccountCode = item.AccountCode
                AccountName = item.AccountName
                Balance = item.Balance
                Percentage = item.Percentage
            }

        {
            FromDate = incomeStatement.FromDate
            ToDate = incomeStatement.ToDate
            Revenues = incomeStatement.Revenues |> List.map mapItem
            Expenses = incomeStatement.Expenses |> List.map mapItem
            GrossProfit = incomeStatement.GrossProfit
            OperatingIncome = incomeStatement.OperatingIncome
            NetIncome = incomeStatement.NetIncome
            TotalRevenues = incomeStatement.TotalRevenues
            TotalExpenses = incomeStatement.TotalExpenses
        }

/// <summary>
/// 財務指標レスポンス DTO
/// </summary>
[<CLIMutable>]
type FinancialRatiosResponse = {
    /// 流動比率（%）
    CurrentRatio: decimal
    /// 自己資本比率（%）
    EquityRatio: decimal
    /// 売上総利益率（%）
    GrossProfitMargin: decimal
    /// 営業利益率（%）
    OperatingProfitMargin: decimal
    /// 当期純利益率（%）
    NetProfitMargin: decimal
    /// 総資産利益率（ROA）（%）
    Roa: decimal
    /// 自己資本利益率（ROE）（%）
    Roe: decimal
}

module FinancialRatiosResponse =
    /// <summary>
    /// Domain Model からの変換
    /// </summary>
    let from (ratios: FinancialRatios) : FinancialRatiosResponse =
        {
            CurrentRatio = ratios.CurrentRatio
            EquityRatio = ratios.EquityRatio
            GrossProfitMargin = ratios.GrossProfitMargin
            OperatingProfitMargin = ratios.OperatingProfitMargin
            NetProfitMargin = ratios.NetProfitMargin
            Roa = ratios.Roa
            Roe = ratios.Roe
        }
