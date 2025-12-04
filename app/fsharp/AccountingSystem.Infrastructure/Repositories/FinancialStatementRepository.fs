module AccountingSystem.Infrastructure.Repositories.FinancialStatementRepository

open System
open Dapper
open Npgsql
open AccountingSystem.Domain.Models
open AccountingSystem.Application.Repositories
open AccountingSystem.Infrastructure.DAO

/// 貸借対照表を生成
let generateBalanceSheetAsync (connectionString: string) (asOfDate: DateTime) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        // 資産・負債・純資産の残高を取得
        // BSPL区分='B'の科目のみを抽出（貸借対照表科目）
        // 取引要素区分: 1=資産, 2=負債, 3=純資産
        let sql = """
            SELECT
                a."勘定科目コード" as account_code,
                a."勘定科目名" as account_name,
                a."BSPL区分" as bspl_type,
                a."取引要素区分" as element_type,
                COALESCE(SUM(d."借方金額"), 0) - COALESCE(SUM(d."貸方金額"), 0) as balance
            FROM "勘定科目マスタ" a
            LEFT JOIN "日次勘定科目残高" d
                ON a."勘定科目コード" = d."勘定科目コード"
            WHERE a."BSPL区分" = 'B'
                AND (d."起票日" <= @AsOfDate OR d."起票日" IS NULL)
            GROUP BY a."勘定科目コード", a."勘定科目名", a."BSPL区分", a."取引要素区分"
            HAVING COALESCE(SUM(d."借方金額"), 0) - COALESCE(SUM(d."貸方金額"), 0) != 0
            ORDER BY a."勘定科目コード"
            """

        let! balances = conn.QueryAsync<BalanceSheetAccountDao>(sql, {| AsOfDate = asOfDate |})

        let mutable assets = []
        let mutable liabilities = []
        let mutable equity = []

        // データを分類
        // 取引要素区分: 1=資産, 2=負債, 3=純資産
        for row in balances do
            let item = BalanceSheetAccountDao.toBalanceSheetItem row

            match row.element_type with
            | "1" -> assets <- item :: assets       // 資産
            | "2" -> liabilities <- item :: liabilities  // 負債
            | "3" -> equity <- item :: equity      // 純資産
            | _ -> ()

        // リストを反転（追加順が逆になっているため）
        assets <- List.rev assets
        liabilities <- List.rev liabilities
        equity <- List.rev equity

        // 合計を計算
        let totalAssets = assets |> List.sumBy (fun item -> item.Balance)
        let totalLiabilities = liabilities |> List.sumBy (fun item -> item.Balance)
        let totalEquity = equity |> List.sumBy (fun item -> item.Balance)
        let totalLiabilitiesAndEquity = totalLiabilities + totalEquity

        // 構成比率を計算
        let calculatePercentage (items: BalanceSheetItem list) (total: decimal) : BalanceSheetItem list =
            items
            |> List.map (fun item ->
                let percentage =
                    if total > 0M then
                        Math.Round((item.Balance / total) * 100M, 2, MidpointRounding.AwayFromZero)
                    else
                        0M
                { item with Percentage = percentage })

        let assetsWithPercentage = calculatePercentage assets totalAssets
        let liabilitiesWithPercentage = calculatePercentage liabilities totalLiabilitiesAndEquity
        let equityWithPercentage = calculatePercentage equity totalLiabilitiesAndEquity

        return {
            AsOfDate = asOfDate
            Assets = assetsWithPercentage
            Liabilities = liabilitiesWithPercentage
            Equity = equityWithPercentage
            TotalAssets = totalAssets
            TotalLiabilities = totalLiabilities
            TotalEquity = totalEquity
            TotalLiabilitiesAndEquity = totalLiabilitiesAndEquity
        }
    }

/// 損益計算書を生成
let generateIncomeStatementAsync (connectionString: string) (fromDate: DateTime) (toDate: DateTime) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        // 収益・費用の残高を取得
        // BSPL区分='P'の科目のみを抽出（損益計算書科目）
        // 取引要素区分: 4=収益, 5=費用
        let sql = """
            SELECT
                a."勘定科目コード" as account_code,
                a."勘定科目名" as account_name,
                a."BSPL区分" as bspl_type,
                a."取引要素区分" as element_type,
                COALESCE(a."費用区分", '0') as expense_type,
                COALESCE(SUM(d."貸方金額"), 0) - COALESCE(SUM(d."借方金額"), 0) as balance
            FROM "勘定科目マスタ" a
            LEFT JOIN "日次勘定科目残高" d
                ON a."勘定科目コード" = d."勘定科目コード"
            WHERE a."BSPL区分" = 'P'
                AND d."起票日" BETWEEN @FromDate AND @ToDate
            GROUP BY a."勘定科目コード", a."勘定科目名", a."BSPL区分", a."取引要素区分", a."費用区分"
            HAVING COALESCE(SUM(d."貸方金額"), 0) - COALESCE(SUM(d."借方金額"), 0) != 0
            ORDER BY a."勘定科目コード"
            """

        let! balances = conn.QueryAsync<IncomeStatementAccountDao>(
            sql, {| FromDate = fromDate; ToDate = toDate |})

        let mutable revenues = []
        let mutable expenses = []

        // データを分類
        // 取引要素区分: 4=収益, 5=費用
        for row in balances do
            let item = IncomeStatementAccountDao.toIncomeStatementItem row

            match row.element_type with
            | "4" -> revenues <- item :: revenues   // 収益
            | "5" -> expenses <- item :: expenses   // 費用
            | _ -> ()

        // リストを反転（追加順が逆になっているため）
        revenues <- List.rev revenues
        expenses <- List.rev expenses

        // 合計を計算
        let totalRevenues = revenues |> List.sumBy (fun item -> item.Balance)
        let totalExpenses = expenses |> List.sumBy (fun item -> item.Balance)

        // 売上原価（勘定科目コードが51で始まる）
        let costOfSales =
            expenses
            |> List.filter (fun e -> e.AccountCode.StartsWith("51"))
            |> List.sumBy (fun e -> e.Balance)

        // 販管費（勘定科目コードが6または52で始まる）
        let operatingExpenses =
            expenses
            |> List.filter (fun e -> e.AccountCode.StartsWith("6") || e.AccountCode.StartsWith("52"))
            |> List.sumBy (fun e -> e.Balance)

        // 利益項目の計算
        let grossProfit = totalRevenues - costOfSales
        let operatingIncome = grossProfit - operatingExpenses
        let netIncome = totalRevenues - totalExpenses

        // 構成比率を計算（対売上比）
        let calculatePercentageForPL (items: IncomeStatementItem list) (totalRevenues: decimal) : IncomeStatementItem list =
            items
            |> List.map (fun item ->
                let percentage =
                    if totalRevenues > 0M then
                        Math.Round((item.Balance / totalRevenues) * 100M, 2, MidpointRounding.AwayFromZero)
                    else
                        0M
                { item with Percentage = percentage })

        let revenuesWithPercentage = calculatePercentageForPL revenues totalRevenues
        let expensesWithPercentage = calculatePercentageForPL expenses totalRevenues

        return {
            FromDate = fromDate
            ToDate = toDate
            Revenues = revenuesWithPercentage
            Expenses = expensesWithPercentage
            GrossProfit = grossProfit
            OperatingIncome = operatingIncome
            NetIncome = netIncome
            TotalRevenues = totalRevenues
            TotalExpenses = totalExpenses
        }
    }

/// <summary>
/// IFinancialStatementRepository インターフェースのアダプター実装
/// module 関数をインターフェース経由で利用可能にする
/// </summary>
type FinancialStatementRepositoryAdapter(connectionString: string) =
    interface IFinancialStatementRepository with
        member _.GenerateBalanceSheetAsync(asOfDate) =
            generateBalanceSheetAsync connectionString asOfDate

        member _.GenerateIncomeStatementAsync(fromDate, toDate) =
            generateIncomeStatementAsync connectionString fromDate toDate
