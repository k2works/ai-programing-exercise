namespace AccountingSystem.Domain.Models

open System

/// <summary>
/// 財務分析用データ
/// 貸借対照表と損益計算書から抽出した分析に必要な科目残高
/// </summary>
type FinancialData = {
    /// 会計年度
    FiscalYear: int
    /// 基準日
    AsOfDate: DateTime

    // 資産の部
    /// 流動資産
    CurrentAssets: decimal
    /// 当座資産（現金預金 + 売掛金 + 売上債権）
    QuickAssets: decimal
    /// 棚卸資産
    Inventory: decimal
    /// 固定資産
    FixedAssets: decimal
    /// 有形固定資産
    TangibleFixedAssets: decimal
    /// 資産合計
    TotalAssets: decimal

    // 負債の部
    /// 流動負債
    CurrentLiabilities: decimal
    /// 固定負債
    FixedLiabilities: decimal
    /// 負債合計
    TotalLiabilities: decimal

    // 純資産の部
    /// 純資産合計（自己資本）
    Equity: decimal

    // 損益の部
    /// 売上高
    NetSales: decimal
    /// 売上原価
    CostOfSales: decimal
    /// 売上総利益
    GrossProfit: decimal
    /// 販売費及び一般管理費
    SellingGeneralAdminExpenses: decimal
    /// 営業利益
    OperatingIncome: decimal
    /// 営業外収益
    NonOperatingIncome: decimal
    /// 営業外費用
    NonOperatingExpenses: decimal
    /// 経常利益
    OrdinaryIncome: decimal
    /// 当期純利益
    NetIncome: decimal
}

/// <summary>
/// 包括的な財務指標
/// D社の事例に基づく収益性・効率性・安全性の各指標
/// </summary>
type ComprehensiveFinancialRatios = {
    /// 会計年度
    FiscalYear: int

    // 収益性分析指標
    /// 売上高総利益率（%）
    GrossProfitMargin: decimal
    /// 売上高営業利益率（%）
    OperatingProfitMargin: decimal
    /// 売上高経常利益率（%）
    OrdinaryProfitMargin: decimal
    /// 売上高販管費比率（%）
    SellingExpenseRatio: decimal
    /// 総資産利益率 ROA（%）
    Roa: decimal
    /// 自己資本利益率 ROE（%）
    Roe: decimal

    // 効率性分析指標
    /// 総資本回転率（回）
    TotalAssetTurnover: decimal
    /// 売上債権回転率（回）
    AccountsReceivableTurnover: decimal
    /// 棚卸資産回転率（回）
    InventoryTurnover: decimal
    /// 有形固定資産回転率（回）
    TangibleFixedAssetTurnover: decimal

    // 安全性分析指標
    /// 流動比率（%）
    CurrentRatio: decimal
    /// 当座比率（%）
    QuickRatio: decimal
    /// 固定比率（%）
    FixedRatio: decimal
    /// 固定長期適合率（%）
    FixedLongTermSuitabilityRatio: decimal
    /// 負債比率（%）
    DebtRatio: decimal
    /// 自己資本比率（%）
    EquityRatio: decimal
}

module FinancialData =
    /// <summary>
    /// 空の財務データを作成
    /// </summary>
    let empty fiscalYear asOfDate : FinancialData = {
        FiscalYear = fiscalYear
        AsOfDate = asOfDate
        CurrentAssets = 0m
        QuickAssets = 0m
        Inventory = 0m
        FixedAssets = 0m
        TangibleFixedAssets = 0m
        TotalAssets = 0m
        CurrentLiabilities = 0m
        FixedLiabilities = 0m
        TotalLiabilities = 0m
        Equity = 0m
        NetSales = 0m
        CostOfSales = 0m
        GrossProfit = 0m
        SellingGeneralAdminExpenses = 0m
        OperatingIncome = 0m
        NonOperatingIncome = 0m
        NonOperatingExpenses = 0m
        OrdinaryIncome = 0m
        NetIncome = 0m
    }

    /// <summary>
    /// 貸借対照表と損益計算書から財務データを構築
    /// </summary>
    let fromStatements (balanceSheet: BalanceSheet) (incomeStatement: IncomeStatement) : FinancialData =
        // 流動資産: コード 11x で始まる
        let currentAssets =
            balanceSheet.Assets
            |> List.filter (fun a -> a.AccountCode.StartsWith("11"))
            |> List.sumBy (fun a -> a.Balance)

        // 当座資産: 現金預金(111) + 売掛金(112) + 売上債権(113)
        let quickAssets =
            balanceSheet.Assets
            |> List.filter (fun a ->
                a.AccountCode = "111" ||
                a.AccountCode = "112" ||
                a.AccountCode = "113")
            |> List.sumBy (fun a -> a.Balance)

        // 棚卸資産: コード 114
        let inventory =
            balanceSheet.Assets
            |> List.filter (fun a -> a.AccountCode = "114")
            |> List.sumBy (fun a -> a.Balance)

        // 固定資産: コード 12x で始まる
        let fixedAssets =
            balanceSheet.Assets
            |> List.filter (fun a -> a.AccountCode.StartsWith("12"))
            |> List.sumBy (fun a -> a.Balance)

        // 有形固定資産: コード 121x で始まる
        let tangibleFixedAssets =
            balanceSheet.Assets
            |> List.filter (fun a -> a.AccountCode.StartsWith("121"))
            |> List.sumBy (fun a -> a.Balance)

        // 流動負債: コード 21x で始まる
        let currentLiabilities =
            balanceSheet.Liabilities
            |> List.filter (fun l -> l.AccountCode.StartsWith("21"))
            |> List.sumBy (fun l -> l.Balance)

        // 固定負債: コード 22x で始まる
        let fixedLiabilities =
            balanceSheet.Liabilities
            |> List.filter (fun l -> l.AccountCode.StartsWith("22"))
            |> List.sumBy (fun l -> l.Balance)

        // 売上高: コード 41
        let netSales =
            incomeStatement.Revenues
            |> List.filter (fun r -> r.AccountCode = "41")
            |> List.sumBy (fun r -> r.Balance)

        // 売上原価: コード 51
        let costOfSales =
            incomeStatement.Expenses
            |> List.filter (fun e -> e.AccountCode = "51")
            |> List.sumBy (fun e -> e.Balance)

        // 販管費: コード 52
        let sellingExpenses =
            incomeStatement.Expenses
            |> List.filter (fun e -> e.AccountCode = "52")
            |> List.sumBy (fun e -> e.Balance)

        // 営業外収益: コード 42
        let nonOperatingIncome =
            incomeStatement.Revenues
            |> List.filter (fun r -> r.AccountCode = "42")
            |> List.sumBy (fun r -> r.Balance)

        // 営業外費用: コード 53
        let nonOperatingExpenses =
            incomeStatement.Expenses
            |> List.filter (fun e -> e.AccountCode = "53")
            |> List.sumBy (fun e -> e.Balance)

        let grossProfit = netSales - costOfSales
        let operatingIncome = grossProfit - sellingExpenses
        let ordinaryIncome = operatingIncome + nonOperatingIncome - nonOperatingExpenses

        {
            FiscalYear = incomeStatement.ToDate.Year
            AsOfDate = balanceSheet.AsOfDate
            CurrentAssets = currentAssets
            QuickAssets = quickAssets
            Inventory = inventory
            FixedAssets = fixedAssets
            TangibleFixedAssets = tangibleFixedAssets
            TotalAssets = balanceSheet.TotalAssets
            CurrentLiabilities = currentLiabilities
            FixedLiabilities = fixedLiabilities
            TotalLiabilities = balanceSheet.TotalLiabilities
            Equity = balanceSheet.TotalEquity
            NetSales = netSales
            CostOfSales = costOfSales
            GrossProfit = grossProfit
            SellingGeneralAdminExpenses = sellingExpenses
            OperatingIncome = operatingIncome
            NonOperatingIncome = nonOperatingIncome
            NonOperatingExpenses = nonOperatingExpenses
            OrdinaryIncome = ordinaryIncome
            NetIncome = incomeStatement.NetIncome
        }

module ComprehensiveFinancialRatios =
    /// <summary>
    /// 安全な除算（ゼロ除算回避）
    /// </summary>
    let private safeDivide numerator denominator decimals =
        if denominator = 0m then 0m
        else Math.Round(numerator / denominator, decimals, MidpointRounding.AwayFromZero)

    /// <summary>
    /// パーセンテージ計算（ゼロ除算回避）
    /// </summary>
    let private safePercentage numerator denominator =
        safeDivide (numerator * 100m) denominator 2

    /// <summary>
    /// 財務データから包括的な財務指標を計算
    /// </summary>
    let calculate (data: FinancialData) : ComprehensiveFinancialRatios =
        // 収益性指標
        let grossProfitMargin = safePercentage data.GrossProfit data.NetSales
        let operatingProfitMargin = safePercentage data.OperatingIncome data.NetSales
        let ordinaryProfitMargin = safePercentage data.OrdinaryIncome data.NetSales
        let sellingExpenseRatio = safePercentage data.SellingGeneralAdminExpenses data.NetSales
        let roa = safePercentage data.OrdinaryIncome data.TotalAssets
        let roe = safePercentage data.NetIncome data.Equity

        // 効率性指標
        let totalAssetTurnover = safeDivide data.NetSales data.TotalAssets 2
        let accountsReceivableTurnover = safeDivide data.NetSales data.QuickAssets 2
        let inventoryTurnover = safeDivide data.NetSales data.Inventory 2
        let tangibleFixedAssetTurnover = safeDivide data.NetSales data.TangibleFixedAssets 2

        // 安全性指標
        let currentRatio = safePercentage data.CurrentAssets data.CurrentLiabilities
        let quickRatio = safePercentage data.QuickAssets data.CurrentLiabilities
        let fixedRatio = safePercentage data.FixedAssets data.Equity
        let fixedLongTermSuitabilityRatio =
            safePercentage data.FixedAssets (data.Equity + data.FixedLiabilities)
        let debtRatio = safePercentage data.TotalLiabilities data.Equity
        let equityRatio = safePercentage data.Equity data.TotalAssets

        {
            FiscalYear = data.FiscalYear
            GrossProfitMargin = grossProfitMargin
            OperatingProfitMargin = operatingProfitMargin
            OrdinaryProfitMargin = ordinaryProfitMargin
            SellingExpenseRatio = sellingExpenseRatio
            Roa = roa
            Roe = roe
            TotalAssetTurnover = totalAssetTurnover
            AccountsReceivableTurnover = accountsReceivableTurnover
            InventoryTurnover = inventoryTurnover
            TangibleFixedAssetTurnover = tangibleFixedAssetTurnover
            CurrentRatio = currentRatio
            QuickRatio = quickRatio
            FixedRatio = fixedRatio
            FixedLongTermSuitabilityRatio = fixedLongTermSuitabilityRatio
            DebtRatio = debtRatio
            EquityRatio = equityRatio
        }

    /// <summary>
    /// 空の財務指標を作成
    /// </summary>
    let empty fiscalYear : ComprehensiveFinancialRatios = {
        FiscalYear = fiscalYear
        GrossProfitMargin = 0m
        OperatingProfitMargin = 0m
        OrdinaryProfitMargin = 0m
        SellingExpenseRatio = 0m
        Roa = 0m
        Roe = 0m
        TotalAssetTurnover = 0m
        AccountsReceivableTurnover = 0m
        InventoryTurnover = 0m
        TangibleFixedAssetTurnover = 0m
        CurrentRatio = 0m
        QuickRatio = 0m
        FixedRatio = 0m
        FixedLongTermSuitabilityRatio = 0m
        DebtRatio = 0m
        EquityRatio = 0m
    }
