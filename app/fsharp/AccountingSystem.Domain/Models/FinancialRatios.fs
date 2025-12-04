namespace AccountingSystem.Domain.Models

open System

/// <summary>
/// 財務指標
/// </summary>
type FinancialRatios = {
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
    /// 総資産利益率（ROA, %）
    Roa: decimal
    /// 自己資本利益率（ROE, %）
    Roe: decimal
}

module FinancialRatios =
    /// <summary>
    /// 財務指標を計算
    /// </summary>
    /// <param name="balanceSheet">貸借対照表</param>
    /// <param name="incomeStatement">損益計算書</param>
    /// <returns>財務指標</returns>
    let calculate (balanceSheet: BalanceSheet) (incomeStatement: IncomeStatement) : FinancialRatios =
        // 流動資産・流動負債の抽出
        // 流動資産: 11（当座資産）または12（棚卸資産）で始まる
        let currentAssets =
            balanceSheet.Assets
            |> List.filter (fun asset ->
                asset.AccountCode.StartsWith("11") || asset.AccountCode.StartsWith("12"))
            |> List.sumBy (fun asset -> asset.Balance)

        // 流動負債: 21で始まる
        let currentLiabilities =
            balanceSheet.Liabilities
            |> List.filter (fun liability -> liability.AccountCode.StartsWith("21"))
            |> List.sumBy (fun liability -> liability.Balance)

        // 各種指標の計算
        let currentRatio =
            if currentLiabilities > 0M then
                Math.Round((currentAssets / currentLiabilities) * 100M, 2, MidpointRounding.AwayFromZero)
            else
                0M

        let equityRatio =
            if balanceSheet.TotalAssets > 0M then
                Math.Round((balanceSheet.TotalEquity / balanceSheet.TotalAssets) * 100M, 2, MidpointRounding.AwayFromZero)
            else
                0M

        let grossProfitMargin =
            if incomeStatement.TotalRevenues > 0M then
                Math.Round((incomeStatement.GrossProfit / incomeStatement.TotalRevenues) * 100M, 2, MidpointRounding.AwayFromZero)
            else
                0M

        let operatingProfitMargin =
            if incomeStatement.TotalRevenues > 0M then
                Math.Round((incomeStatement.OperatingIncome / incomeStatement.TotalRevenues) * 100M, 2, MidpointRounding.AwayFromZero)
            else
                0M

        let netProfitMargin =
            if incomeStatement.TotalRevenues > 0M then
                Math.Round((incomeStatement.NetIncome / incomeStatement.TotalRevenues) * 100M, 2, MidpointRounding.AwayFromZero)
            else
                0M

        let roa =
            if balanceSheet.TotalAssets > 0M then
                Math.Round((incomeStatement.NetIncome / balanceSheet.TotalAssets) * 100M, 2, MidpointRounding.AwayFromZero)
            else
                0M

        let roe =
            if balanceSheet.TotalEquity > 0M then
                Math.Round((incomeStatement.NetIncome / balanceSheet.TotalEquity) * 100M, 2, MidpointRounding.AwayFromZero)
            else
                0M

        {
            CurrentRatio = currentRatio
            EquityRatio = equityRatio
            GrossProfitMargin = grossProfitMargin
            OperatingProfitMargin = operatingProfitMargin
            NetProfitMargin = netProfitMargin
            Roa = roa
            Roe = roe
        }

    /// <summary>
    /// 空の財務指標を作成
    /// </summary>
    let empty = {
        CurrentRatio = 0M
        EquityRatio = 0M
        GrossProfitMargin = 0M
        OperatingProfitMargin = 0M
        NetProfitMargin = 0M
        Roa = 0M
        Roe = 0M
    }
