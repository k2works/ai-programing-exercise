using AccountingSystem.Domain.Financial;
using AccountingSystem.Infrastructure.Repositories;
using Dapper;
using Npgsql;

namespace AccountingSystem.Application.Services;

/// <summary>
/// 財務諸表生成サービス
/// </summary>
public class FinancialStatementService : IFinancialStatementService
{
    private readonly string _connectionString;

    static FinancialStatementService()
    {
        SqlMapper.AddTypeHandler(new DateOnlyTypeHandler());
    }

    public FinancialStatementService(string connectionString)
    {
        _connectionString = connectionString;
    }

    /// <summary>
    /// 貸借対照表を生成
    /// </summary>
    /// <param name="asOfDate">基準日</param>
    /// <returns>貸借対照表</returns>
    public async Task<BalanceSheet> GenerateBalanceSheetAsync(DateOnly asOfDate)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.OpenAsync();

        // 資産・負債・純資産の残高を取得
        // 取引要素区分: 1=資産, 2=負債, 3=純資産, 4=収益, 5=費用
        var sql = @"
            SELECT
                a.""勘定科目コード"" as account_code,
                a.""勘定科目名"" as account_name,
                a.""BSPL区分"" as bspl_type,
                a.""取引要素区分"" as element_type,
                COALESCE(SUM(d.""借方金額""), 0) - COALESCE(SUM(d.""貸方金額""), 0) as balance
            FROM ""勘定科目マスタ"" a
            LEFT JOIN ""日次勘定科目残高"" d
                ON a.""勘定科目コード"" = d.""勘定科目コード""
                AND d.""起票日"" <= @AsOfDate
            WHERE a.""BSPL区分"" = 'B'
            GROUP BY a.""勘定科目コード"", a.""勘定科目名"", a.""BSPL区分"", a.""取引要素区分""
            HAVING COALESCE(SUM(d.""借方金額""), 0) - COALESCE(SUM(d.""貸方金額""), 0) != 0
            ORDER BY a.""勘定科目コード""
        ";

        var balances = await connection.QueryAsync<dynamic>(sql, new { AsOfDate = asOfDate });

        var assets = new List<BalanceSheetItem>();
        var liabilities = new List<BalanceSheetItem>();
        var equity = new List<BalanceSheetItem>();

        // データを分類
        foreach (var row in balances)
        {
            var accountCode = (string)row.account_code;
            var accountName = (string)row.account_name;
            var balance = (decimal)row.balance;
            var elementType = ((string)row.element_type).Trim();

            var item = new BalanceSheetItem
            {
                AccountCode = accountCode,
                AccountName = accountName,
                Balance = Math.Abs(balance),
                Percentage = 0m // 後で計算
            };

            // 取引要素区分で分類
            switch (elementType)
            {
                case "1": // 資産
                    assets.Add(item);
                    break;
                case "2": // 負債
                    liabilities.Add(item);
                    break;
                case "3": // 純資産
                    equity.Add(item);
                    break;
            }
        }

        // 合計を計算
        var totalAssets = assets.Sum(item => item.Balance);
        var totalLiabilities = liabilities.Sum(item => item.Balance);
        var totalEquity = equity.Sum(item => item.Balance);
        var totalLiabilitiesAndEquity = totalLiabilities + totalEquity;

        // 構成比率を計算
        var assetsWithPercentage = CalculatePercentage(assets, totalAssets);
        var liabilitiesWithPercentage = CalculatePercentage(liabilities, totalLiabilitiesAndEquity);
        var equityWithPercentage = CalculatePercentage(equity, totalLiabilitiesAndEquity);

        return new BalanceSheet
        {
            AsOfDate = asOfDate,
            Assets = assetsWithPercentage,
            Liabilities = liabilitiesWithPercentage,
            Equity = equityWithPercentage,
            TotalAssets = totalAssets,
            TotalLiabilities = totalLiabilities,
            TotalEquity = totalEquity,
            TotalLiabilitiesAndEquity = totalLiabilitiesAndEquity
        };
    }

    /// <summary>
    /// 損益計算書を生成
    /// </summary>
    /// <param name="fromDate">開始日</param>
    /// <param name="toDate">終了日</param>
    /// <returns>損益計算書</returns>
    public async Task<IncomeStatement> GenerateIncomeStatementAsync(DateOnly fromDate, DateOnly toDate)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.OpenAsync();

        // 収益・費用の残高を取得
        // 取引要素区分: 4=収益, 5=費用
        var sql = @"
            SELECT
                a.""勘定科目コード"" as account_code,
                a.""勘定科目名"" as account_name,
                a.""BSPL区分"" as bspl_type,
                a.""取引要素区分"" as element_type,
                a.""費用区分"" as expense_type,
                CASE
                    WHEN a.""取引要素区分"" = '4' THEN
                        COALESCE(SUM(d.""貸方金額""), 0) - COALESCE(SUM(d.""借方金額""), 0)
                    ELSE
                        COALESCE(SUM(d.""借方金額""), 0) - COALESCE(SUM(d.""貸方金額""), 0)
                END as amount
            FROM ""勘定科目マスタ"" a
            LEFT JOIN ""日次勘定科目残高"" d
                ON a.""勘定科目コード"" = d.""勘定科目コード""
                AND d.""起票日"" BETWEEN @FromDate AND @ToDate
            WHERE a.""BSPL区分"" = 'P'
            GROUP BY a.""勘定科目コード"", a.""勘定科目名"", a.""BSPL区分"", a.""取引要素区分"", a.""費用区分""
            HAVING CASE
                    WHEN a.""取引要素区分"" = '4' THEN
                        COALESCE(SUM(d.""貸方金額""), 0) - COALESCE(SUM(d.""借方金額""), 0)
                    ELSE
                        COALESCE(SUM(d.""借方金額""), 0) - COALESCE(SUM(d.""貸方金額""), 0)
                END != 0
            ORDER BY a.""勘定科目コード""
        ";

        var balances = await connection.QueryAsync<dynamic>(sql, new { FromDate = fromDate, ToDate = toDate });

        var revenues = new List<IncomeStatementItem>();
        var expenses = new List<IncomeStatementItem>();
        decimal costOfSales = 0m;
        decimal operatingExpenses = 0m;

        // データを分類
        foreach (var row in balances)
        {
            var accountCode = (string)row.account_code;
            var accountName = (string)row.account_name;
            var amount = (decimal)row.amount;
            var elementType = ((string)row.element_type).Trim();
            var expenseType = row.expense_type != null ? ((string)row.expense_type).Trim() : null;

            var item = new IncomeStatementItem
            {
                AccountCode = accountCode,
                AccountName = accountName,
                Amount = Math.Abs(amount),
                Percentage = 0m // 後で計算
            };

            // 取引要素区分で分類
            if (elementType == "4") // 収益
            {
                revenues.Add(item);
            }
            else if (elementType == "5") // 費用
            {
                expenses.Add(item);

                // 費用区分で売上原価・販管費を分類
                if (expenseType == "1") // 売上原価
                {
                    costOfSales += item.Amount;
                }
                else if (expenseType == "2") // 販管費
                {
                    operatingExpenses += item.Amount;
                }
            }
        }

        // 合計を計算
        var totalRevenues = revenues.Sum(item => item.Amount);
        var totalExpenses = expenses.Sum(item => item.Amount);

        // 利益項目の計算
        var grossProfit = totalRevenues - costOfSales;
        var operatingIncome = grossProfit - operatingExpenses;
        var netIncome = totalRevenues - totalExpenses;

        // 構成比率を計算（対売上比）
        var revenuesWithPercentage = CalculatePercentageForPL(revenues, totalRevenues);
        var expensesWithPercentage = CalculatePercentageForPL(expenses, totalRevenues);

        return new IncomeStatement
        {
            FromDate = fromDate,
            ToDate = toDate,
            Revenues = revenuesWithPercentage,
            Expenses = expensesWithPercentage,
            GrossProfit = grossProfit,
            OperatingIncome = operatingIncome,
            NetIncome = netIncome,
            TotalRevenues = totalRevenues,
            TotalExpenses = totalExpenses
        };
    }

    /// <summary>
    /// 構成比率を計算（貸借対照表用）
    /// </summary>
    private static List<BalanceSheetItem> CalculatePercentage(
        IEnumerable<BalanceSheetItem> items,
        decimal total)
    {
        return items.Select(item =>
        {
            var percentage = total > 0m
                ? Math.Round((item.Balance / total) * 100m, 2, MidpointRounding.AwayFromZero)
                : 0m;

            return new BalanceSheetItem
            {
                AccountCode = item.AccountCode,
                AccountName = item.AccountName,
                Balance = item.Balance,
                Percentage = percentage
            };
        }).ToList();
    }

    /// <summary>
    /// 構成比率を計算（損益計算書用・対売上比）
    /// </summary>
    private static List<IncomeStatementItem> CalculatePercentageForPL(
        IEnumerable<IncomeStatementItem> items,
        decimal totalRevenues)
    {
        return items.Select(item =>
        {
            var percentage = totalRevenues > 0m
                ? Math.Round((item.Amount / totalRevenues) * 100m, 2, MidpointRounding.AwayFromZero)
                : 0m;

            return new IncomeStatementItem
            {
                AccountCode = item.AccountCode,
                AccountName = item.AccountName,
                Amount = item.Amount,
                Percentage = percentage
            };
        }).ToList();
    }

    /// <summary>
    /// 財務指標を計算
    /// </summary>
    /// <param name="balanceSheet">貸借対照表</param>
    /// <param name="incomeStatement">損益計算書</param>
    /// <returns>財務指標</returns>
    public FinancialRatios CalculateFinancialRatios(
        BalanceSheet balanceSheet,
        IncomeStatement incomeStatement)
    {
        // 流動資産（勘定科目コードが11または12で始まる）
        var currentAssets = balanceSheet.Assets
            .Where(a => a.AccountCode.StartsWith("11", StringComparison.Ordinal) ||
                        a.AccountCode.StartsWith("12", StringComparison.Ordinal))
            .Sum(a => a.Balance);

        // 流動負債（勘定科目コードが21で始まる）
        var currentLiabilities = balanceSheet.Liabilities
            .Where(l => l.AccountCode.StartsWith("21", StringComparison.Ordinal))
            .Sum(l => l.Balance);

        // 流動比率
        var currentRatio = currentLiabilities > 0m
            ? Math.Round((currentAssets / currentLiabilities) * 100m, 2, MidpointRounding.AwayFromZero)
            : 0m;

        // 自己資本比率
        var equityRatio = balanceSheet.TotalAssets > 0m
            ? Math.Round((balanceSheet.TotalEquity / balanceSheet.TotalAssets) * 100m, 2, MidpointRounding.AwayFromZero)
            : 0m;

        // 売上総利益率
        var grossProfitMargin = incomeStatement.TotalRevenues > 0m
            ? Math.Round((incomeStatement.GrossProfit / incomeStatement.TotalRevenues) * 100m, 2, MidpointRounding.AwayFromZero)
            : 0m;

        // 営業利益率
        var operatingProfitMargin = incomeStatement.TotalRevenues > 0m
            ? Math.Round((incomeStatement.OperatingIncome / incomeStatement.TotalRevenues) * 100m, 2, MidpointRounding.AwayFromZero)
            : 0m;

        // 当期純利益率
        var netProfitMargin = incomeStatement.TotalRevenues > 0m
            ? Math.Round((incomeStatement.NetIncome / incomeStatement.TotalRevenues) * 100m, 2, MidpointRounding.AwayFromZero)
            : 0m;

        // ROA（総資産利益率）
        var roa = balanceSheet.TotalAssets > 0m
            ? Math.Round((incomeStatement.NetIncome / balanceSheet.TotalAssets) * 100m, 2, MidpointRounding.AwayFromZero)
            : 0m;

        // ROE（自己資本利益率）
        var roe = balanceSheet.TotalEquity > 0m
            ? Math.Round((incomeStatement.NetIncome / balanceSheet.TotalEquity) * 100m, 2, MidpointRounding.AwayFromZero)
            : 0m;

        return new FinancialRatios
        {
            CurrentRatio = currentRatio,
            EquityRatio = equityRatio,
            GrossProfitMargin = grossProfitMargin,
            OperatingProfitMargin = operatingProfitMargin,
            NetProfitMargin = netProfitMargin,
            Roa = roa,
            Roe = roe
        };
    }
}
