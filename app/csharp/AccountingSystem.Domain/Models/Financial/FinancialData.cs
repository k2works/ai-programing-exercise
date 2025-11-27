namespace AccountingSystem.Domain.Models.Financial;

/// <summary>
/// 財務データを表現する値オブジェクト
/// このクラスは不変であり、スレッドセーフです。
/// </summary>
public record FinancialData(
    int FiscalYear,
    decimal Sales,
    decimal CostOfSales,
    decimal SellingExpenses,
    decimal OperatingProfit,
    decimal OrdinaryProfit,
    decimal NetProfit,
    decimal TotalAssets,
    decimal TangibleFixedAssets,
    decimal CurrentAssets,
    decimal CurrentLiabilities,
    decimal FixedLiabilities,
    decimal QuickAssets,
    decimal Equity,
    decimal Inventory,
    decimal ReceivablesTurnover
)
{
    /// <summary>
    /// 売上総利益（粗利益）
    /// </summary>
    public decimal GrossProfit => Sales - CostOfSales;

    /// <summary>
    /// 総負債
    /// </summary>
    public decimal TotalLiabilities => CurrentLiabilities + FixedLiabilities;

    /// <summary>
    /// 固定資産
    /// </summary>
    public decimal FixedAssets => TotalAssets - CurrentAssets;

    /// <summary>
    /// 月次勘定科目残高データからFinancialDataを生成
    /// </summary>
    public static FinancialData FromMonthlyBalances(
        int fiscalYear,
        IEnumerable<MonthlyBalanceData> balances)
    {
        var balanceList = balances.ToList();

        // 各勘定科目の金額を取得するヘルパー関数
        decimal GetDebitAmount(string code)
        {
            var balance = balanceList.FirstOrDefault(b => b.AccountCode == code);
            return balance?.DebitAmount ?? 0m;
        }

        decimal GetCreditAmount(string code)
        {
            var balance = balanceList.FirstOrDefault(b => b.AccountCode == code);
            return balance?.CreditAmount ?? 0m;
        }

        // 貸借対照表項目（借方科目は借方金額、貸方科目は貸方金額）
        var currentAssets = GetDebitAmount("11");           // 流動資産
        var cashAndDeposits = GetDebitAmount("111");        // 現金預金
        var receivables = GetDebitAmount("112");            // 売掛金
        var inventory = GetDebitAmount("114");              // 棚卸資産
        var fixedAssets = GetDebitAmount("12");             // 固定資産
        var tangibleFixedAssets = GetDebitAmount("121");    // 有形固定資産（建物等を含む）
        var totalAssets = currentAssets + fixedAssets;      // 総資産

        var currentLiabilities = GetCreditAmount("21");     // 流動負債
        var fixedLiabilities = GetCreditAmount("22");       // 固定負債

        var capitalStock = GetCreditAmount("31");           // 資本金
        var retainedEarnings = GetCreditAmount("33");       // 利益剰余金
        var equity = capitalStock + retainedEarnings;       // 純資産

        // 損益計算書項目（収益は貸方、費用は借方）
        var sales = GetCreditAmount("41");                  // 売上高
        var costOfSales = GetDebitAmount("51");             // 売上原価
        var sellingExpenses = GetDebitAmount("52");         // 販売費及び一般管理費
        var nonOperatingIncome = GetCreditAmount("42");     // 営業外収益
        var nonOperatingExpense = GetDebitAmount("53");     // 営業外費用
        var taxExpense = GetDebitAmount("55");              // 法人税等

        // 利益計算
        var operatingProfit = sales - costOfSales - sellingExpenses;
        var ordinaryProfit = operatingProfit + nonOperatingIncome - nonOperatingExpense;
        var netProfit = ordinaryProfit - taxExpense;

        // 当座資産 = 流動資産 - 棚卸資産
        var quickAssets = currentAssets - inventory;

        // 売上債権回転率 = 売上高 / 売上債権
        var receivablesTurnover = receivables > 0 ? sales / receivables : 0m;

        return new FinancialData(
            fiscalYear,
            sales,
            costOfSales,
            sellingExpenses,
            operatingProfit,
            ordinaryProfit,
            netProfit,
            totalAssets,
            tangibleFixedAssets,
            currentAssets,
            currentLiabilities,
            fixedLiabilities,
            quickAssets,
            equity,
            inventory,
            receivablesTurnover
        );
    }
}

/// <summary>
/// 月次勘定科目残高データ
/// </summary>
public record MonthlyBalanceData(
    string AccountCode,
    decimal DebitAmount,
    decimal CreditAmount
);
