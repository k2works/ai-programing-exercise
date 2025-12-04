module AccountingSystem.Tests.Repositories.FinancialStatementRepositoryTest

open System
open Xunit
open FsUnit.Xunit
open Npgsql
open AccountingSystem.Tests.DatabaseTestBase
open AccountingSystem.Domain.Models
open AccountingSystem.Application.Repositories
open AccountingSystem.Infrastructure.Repositories.FinancialStatementRepository

/// <summary>
/// FinancialStatementRepository（財務諸表リポジトリ）のテスト
/// </summary>
type FinancialStatementRepositoryTest() =
    inherit DatabaseTestBase()

    /// リポジトリを取得
    member private this.CreateRepository() : IFinancialStatementRepository =
        FinancialStatementRepositoryAdapter(this.ConnectionString)

    /// 貸借対照表用テストデータをセットアップ
    member private this.SetupBalanceSheetTestDataAsync() =
        task {
            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()

            use cmd = new NpgsqlCommand("""
                DELETE FROM "日次勘定科目残高";

                INSERT INTO "勘定科目マスタ" ("勘定科目コード", "勘定科目名", "勘定科目種別", "残高", "BSPL区分", "取引要素区分")
                VALUES ('1110', '普通預金', '資産', 0, 'B', '1')
                ON CONFLICT ("勘定科目コード") DO UPDATE SET "BSPL区分" = 'B', "取引要素区分" = '1';

                INSERT INTO "勘定科目マスタ" ("勘定科目コード", "勘定科目名", "勘定科目種別", "残高", "BSPL区分", "取引要素区分")
                VALUES ('1410', '建物', '資産', 0, 'B', '1')
                ON CONFLICT ("勘定科目コード") DO UPDATE SET "BSPL区分" = 'B', "取引要素区分" = '1';

                INSERT INTO "勘定科目マスタ" ("勘定科目コード", "勘定科目名", "勘定科目種別", "残高", "BSPL区分", "取引要素区分")
                VALUES ('2110', '買掛金', '負債', 0, 'B', '2')
                ON CONFLICT ("勘定科目コード") DO UPDATE SET "BSPL区分" = 'B', "取引要素区分" = '2';

                INSERT INTO "勘定科目マスタ" ("勘定科目コード", "勘定科目名", "勘定科目種別", "残高", "BSPL区分", "取引要素区分")
                VALUES ('2510', '長期借入金', '負債', 0, 'B', '2')
                ON CONFLICT ("勘定科目コード") DO UPDATE SET "BSPL区分" = 'B', "取引要素区分" = '2';

                INSERT INTO "勘定科目マスタ" ("勘定科目コード", "勘定科目名", "勘定科目種別", "残高", "BSPL区分", "取引要素区分")
                VALUES ('3110', '資本金', '純資産', 0, 'B', '3')
                ON CONFLICT ("勘定科目コード") DO UPDATE SET "BSPL区分" = 'B', "取引要素区分" = '3';
            """, conn)
            let! _ = cmd.ExecuteNonQueryAsync()

            let asOfDate = DateTime(2024, 1, 31)
            use insertCmd = new NpgsqlCommand("""
                INSERT INTO "日次勘定科目残高" (
                    "起票日", "勘定科目コード", "補助科目コード", "部門コード",
                    "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
                ) VALUES
                (@AsOfDate, '1110', '', '', '', 0, 8000000, 0),
                (@AsOfDate, '1410', '', '', '', 0, 2000000, 0),
                (@AsOfDate, '2110', '', '', '', 0, 0, 500000),
                (@AsOfDate, '2510', '', '', '', 0, 0, 4500000),
                (@AsOfDate, '3110', '', '', '', 0, 0, 5000000)
            """, conn)
            insertCmd.Parameters.AddWithValue("AsOfDate", asOfDate) |> ignore
            let! _ = insertCmd.ExecuteNonQueryAsync()
            ()
        }

    /// 損益計算書用テストデータをセットアップ
    member private this.SetupIncomeStatementTestDataAsync() =
        task {
            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()

            use cmd = new NpgsqlCommand("""
                DELETE FROM "日次勘定科目残高";

                INSERT INTO "勘定科目マスタ" ("勘定科目コード", "勘定科目名", "勘定科目種別", "残高", "BSPL区分", "取引要素区分", "費用区分")
                VALUES ('4110', '売上高', '収益', 0, 'P', '4', '0')
                ON CONFLICT ("勘定科目コード") DO UPDATE SET "BSPL区分" = 'P', "取引要素区分" = '4', "費用区分" = '0';

                INSERT INTO "勘定科目マスタ" ("勘定科目コード", "勘定科目名", "勘定科目種別", "残高", "BSPL区分", "取引要素区分", "費用区分")
                VALUES ('5110', '売上原価', '費用', 0, 'P', '5', '1')
                ON CONFLICT ("勘定科目コード") DO UPDATE SET "BSPL区分" = 'P', "取引要素区分" = '5', "費用区分" = '1';

                INSERT INTO "勘定科目マスタ" ("勘定科目コード", "勘定科目名", "勘定科目種別", "残高", "BSPL区分", "取引要素区分", "費用区分")
                VALUES ('6110', '給与手当', '費用', 0, 'P', '5', '2')
                ON CONFLICT ("勘定科目コード") DO UPDATE SET "BSPL区分" = 'P', "取引要素区分" = '5', "費用区分" = '2';

                INSERT INTO "勘定科目マスタ" ("勘定科目コード", "勘定科目名", "勘定科目種別", "残高", "BSPL区分", "取引要素区分", "費用区分")
                VALUES ('6120', '法定福利費', '費用', 0, 'P', '5', '2')
                ON CONFLICT ("勘定科目コード") DO UPDATE SET "BSPL区分" = 'P', "取引要素区分" = '5', "費用区分" = '2';
            """, conn)
            let! _ = cmd.ExecuteNonQueryAsync()

            use insertCmd = new NpgsqlCommand("""
                INSERT INTO "日次勘定科目残高" (
                    "起票日", "勘定科目コード", "補助科目コード", "部門コード",
                    "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
                ) VALUES
                (@StartDate, '4110', '', '', '', 0, 0, 10000000),
                (@StartDate, '5110', '', '', '', 0, 6000000, 0),
                (@StartDate, '6110', '', '', '', 0, 2000000, 0),
                (@StartDate, '6120', '', '', '', 0, 500000, 0)
            """, conn)
            insertCmd.Parameters.AddWithValue("StartDate", DateTime(2024, 1, 15)) |> ignore
            let! _ = insertCmd.ExecuteNonQueryAsync()
            ()
        }

    // ===========================================
    // 貸借対照表テスト
    // ===========================================

    [<Fact>]
    member this.``貸借対照表を生成できる``() =
        task {
            do! this.SetupBalanceSheetTestDataAsync()
            let asOfDate = DateTime(2024, 1, 31)
            let repository = this.CreateRepository()

            let! balanceSheet = repository.GenerateBalanceSheetAsync(asOfDate)

            balanceSheet.AsOfDate |> should equal asOfDate
            balanceSheet.Assets |> should not' (be Empty)
            balanceSheet.Liabilities |> should not' (be Empty)
            balanceSheet.Equity |> should not' (be Empty)
            balanceSheet.TotalAssets |> should be (greaterThan 0M)
            balanceSheet.TotalLiabilities |> should be (greaterThan 0M)
            balanceSheet.TotalEquity |> should be (greaterThan 0M)
        }

    [<Fact>]
    member this.``貸借平均の原則が成立している``() =
        task {
            do! this.SetupBalanceSheetTestDataAsync()
            let asOfDate = DateTime(2024, 1, 31)
            let repository = this.CreateRepository()

            let! balanceSheet = repository.GenerateBalanceSheetAsync(asOfDate)

            let expectedTotal = balanceSheet.TotalLiabilities + balanceSheet.TotalEquity
            balanceSheet.TotalAssets |> should equal expectedTotal
            balanceSheet.TotalAssets |> should equal balanceSheet.TotalLiabilitiesAndEquity
            balanceSheet.TotalAssets |> should equal 10000000M
            balanceSheet.TotalLiabilities |> should equal 5000000M
            balanceSheet.TotalEquity |> should equal 5000000M
        }

    [<Fact>]
    member this.``資産項目が正しく分類されている``() =
        task {
            do! this.SetupBalanceSheetTestDataAsync()
            let asOfDate = DateTime(2024, 1, 31)
            let repository = this.CreateRepository()

            let! balanceSheet = repository.GenerateBalanceSheetAsync(asOfDate)

            let assets = balanceSheet.Assets
            assets |> should haveLength 2

            let currentAssets = assets |> List.filter (fun a -> a.AccountCode.StartsWith("11"))
            currentAssets |> should haveLength 1
            currentAssets.[0].AccountCode |> should equal "1110"
            currentAssets.[0].Balance |> should equal 8000000M

            let fixedAssets = assets |> List.filter (fun a -> a.AccountCode.StartsWith("14"))
            fixedAssets |> should haveLength 1
            fixedAssets.[0].AccountCode |> should equal "1410"
            fixedAssets.[0].Balance |> should equal 2000000M
        }

    [<Fact>]
    member this.``負債項目が正しく分類されている``() =
        task {
            do! this.SetupBalanceSheetTestDataAsync()
            let asOfDate = DateTime(2024, 1, 31)
            let repository = this.CreateRepository()

            let! balanceSheet = repository.GenerateBalanceSheetAsync(asOfDate)

            let liabilities = balanceSheet.Liabilities
            liabilities |> should haveLength 2

            let currentLiabilities = liabilities |> List.filter (fun l -> l.AccountCode.StartsWith("21"))
            currentLiabilities |> should haveLength 1
            currentLiabilities.[0].Balance |> should equal 500000M

            let longTermLiabilities = liabilities |> List.filter (fun l -> l.AccountCode.StartsWith("25"))
            longTermLiabilities |> should haveLength 1
            longTermLiabilities.[0].Balance |> should equal 4500000M
        }

    [<Fact>]
    member this.``純資産項目が正しく分類されている``() =
        task {
            do! this.SetupBalanceSheetTestDataAsync()
            let asOfDate = DateTime(2024, 1, 31)
            let repository = this.CreateRepository()

            let! balanceSheet = repository.GenerateBalanceSheetAsync(asOfDate)

            let equity = balanceSheet.Equity
            equity |> should haveLength 1
            equity.[0].AccountCode |> should equal "3110"
            equity.[0].Balance |> should equal 5000000M
        }

    [<Fact>]
    member this.``貸借対照表の構成比率が計算されている``() =
        task {
            do! this.SetupBalanceSheetTestDataAsync()
            let asOfDate = DateTime(2024, 1, 31)
            let repository = this.CreateRepository()

            let! balanceSheet = repository.GenerateBalanceSheetAsync(asOfDate)

            balanceSheet.Assets |> List.iter (fun a ->
                a.Percentage |> should be (greaterThanOrEqualTo 0M)
                a.Percentage |> should be (lessThanOrEqualTo 100M))

            let ordinaryDeposit = balanceSheet.Assets |> List.find (fun a -> a.AccountCode = "1110")
            ordinaryDeposit.Percentage |> should equal 80.00M
        }

    // ===========================================
    // 損益計算書テスト
    // ===========================================

    [<Fact>]
    member this.``損益計算書を生成できる``() =
        task {
            do! this.SetupIncomeStatementTestDataAsync()
            let fromDate = DateTime(2024, 1, 1)
            let toDate = DateTime(2024, 1, 31)
            let repository = this.CreateRepository()

            let! incomeStatement = repository.GenerateIncomeStatementAsync(fromDate, toDate)

            incomeStatement.FromDate |> should equal fromDate
            incomeStatement.ToDate |> should equal toDate
            incomeStatement.Revenues |> should not' (be Empty)
            incomeStatement.Expenses |> should not' (be Empty)
            incomeStatement.TotalRevenues |> should be (greaterThan 0M)
            incomeStatement.TotalExpenses |> should be (greaterThan 0M)
        }

    [<Fact>]
    member this.``当期純利益が正しく計算される``() =
        task {
            do! this.SetupIncomeStatementTestDataAsync()
            let fromDate = DateTime(2024, 1, 1)
            let toDate = DateTime(2024, 1, 31)
            let repository = this.CreateRepository()

            let! incomeStatement = repository.GenerateIncomeStatementAsync(fromDate, toDate)

            let expectedNetIncome = incomeStatement.TotalRevenues - incomeStatement.TotalExpenses
            incomeStatement.NetIncome |> should equal expectedNetIncome
            incomeStatement.TotalRevenues |> should equal 10000000M
            incomeStatement.TotalExpenses |> should equal 8500000M
            incomeStatement.NetIncome |> should equal 1500000M
        }

    [<Fact>]
    member this.``売上総利益が正しく計算される``() =
        task {
            do! this.SetupIncomeStatementTestDataAsync()
            let fromDate = DateTime(2024, 1, 1)
            let toDate = DateTime(2024, 1, 31)
            let repository = this.CreateRepository()

            let! incomeStatement = repository.GenerateIncomeStatementAsync(fromDate, toDate)

            incomeStatement.GrossProfit |> should equal 4000000M
        }

    [<Fact>]
    member this.``営業利益が正しく計算される``() =
        task {
            do! this.SetupIncomeStatementTestDataAsync()
            let fromDate = DateTime(2024, 1, 1)
            let toDate = DateTime(2024, 1, 31)
            let repository = this.CreateRepository()

            let! incomeStatement = repository.GenerateIncomeStatementAsync(fromDate, toDate)

            incomeStatement.OperatingIncome |> should equal 1500000M
        }

    [<Fact>]
    member this.``収益項目が正しく分類されている``() =
        task {
            do! this.SetupIncomeStatementTestDataAsync()
            let fromDate = DateTime(2024, 1, 1)
            let toDate = DateTime(2024, 1, 31)
            let repository = this.CreateRepository()

            let! incomeStatement = repository.GenerateIncomeStatementAsync(fromDate, toDate)

            let revenues = incomeStatement.Revenues
            revenues |> should haveLength 1
            revenues.[0].AccountCode |> should equal "4110"
            revenues.[0].Balance |> should equal 10000000M
        }

    [<Fact>]
    member this.``費用項目が正しく分類されている``() =
        task {
            do! this.SetupIncomeStatementTestDataAsync()
            let fromDate = DateTime(2024, 1, 1)
            let toDate = DateTime(2024, 1, 31)
            let repository = this.CreateRepository()

            let! incomeStatement = repository.GenerateIncomeStatementAsync(fromDate, toDate)

            let expenses = incomeStatement.Expenses
            expenses |> should haveLength 3

            let costOfSales = expenses |> List.find (fun e -> e.AccountCode = "5110")
            costOfSales.Balance |> should equal 6000000M

            let salaries = expenses |> List.find (fun e -> e.AccountCode = "6110")
            salaries.Balance |> should equal 2000000M

            let legalWelfare = expenses |> List.find (fun e -> e.AccountCode = "6120")
            legalWelfare.Balance |> should equal 500000M
        }

    [<Fact>]
    member this.``損益計算書の対売上比率が計算されている``() =
        task {
            do! this.SetupIncomeStatementTestDataAsync()
            let fromDate = DateTime(2024, 1, 1)
            let toDate = DateTime(2024, 1, 31)
            let repository = this.CreateRepository()

            let! incomeStatement = repository.GenerateIncomeStatementAsync(fromDate, toDate)

            incomeStatement.Expenses |> List.iter (fun e ->
                e.Percentage |> should be (greaterThanOrEqualTo 0M)
                e.Percentage |> should be (lessThanOrEqualTo 100M))

            let costOfSales = incomeStatement.Expenses |> List.find (fun e -> e.AccountCode = "5110")
            costOfSales.Percentage |> should equal 60.00M
        }
