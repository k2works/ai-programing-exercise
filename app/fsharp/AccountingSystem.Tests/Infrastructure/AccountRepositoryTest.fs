module AccountingSystem.Tests.Infrastructure.AccountRepositoryTest

open AccountingSystem.Domain.Models
open AccountingSystem.Domain.Types
open AccountingSystem.Infrastructure.Persistence.Repositories.AccountRepository
open AccountingSystem.Tests.DatabaseTestBase
open Npgsql
open Xunit
open FsUnit.Xunit
open System

/// <summary>
/// 勘定科目マスタ - Dapper 統合テスト
/// </summary>
type AccountRepositoryTest() =
    inherit DatabaseTestBase()

    /// テストデータをクリーンアップ
    member private this.CleanupTestAccountsAsync() =
        task {
            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()

            // テスト用勘定科目を削除（テスト用に追加したもののみ）
            use cmd = new NpgsqlCommand("""
                DELETE FROM "勘定科目構成マスタ" WHERE "勘定科目コード" LIKE 'TEST%';
                DELETE FROM "勘定科目マスタ" WHERE "勘定科目コード" LIKE 'TEST%';
            """, conn)
            let! _ = cmd.ExecuteNonQueryAsync()
            ()
        }

    [<Fact>]
    member this.``勘定科目を登録できる``() =
        task {
            do! this.CleanupTestAccountsAsync()

            let account = Account.create "TEST001" "テスト勘定科目" AccountType.Asset false
            let account = { account with Balance = Money.Create(50000m) }

            let! accountId = insertAsync this.ConnectionString account
            accountId |> should be (greaterThan 0)

            // 登録を確認
            let! found = findByCodeAsync this.ConnectionString "TEST001"
            found.IsSome |> should equal true
            found.Value.AccountName |> should equal "テスト勘定科目"
            found.Value.Balance.Amount |> should equal 50000m

            // クリーンアップ
            let! _ = deleteAsync this.ConnectionString "TEST001"
            ()
        }

    [<Fact>]
    member this.``勘定科目コードで検索できる``() =
        task {
            do! this.CleanupTestAccountsAsync()

            let account = Account.create "TEST002" "検索テスト科目" AccountType.Liability false
            let account = { account with Balance = Money.Create(100000m) }

            let! _ = insertAsync this.ConnectionString account

            let! found = findByCodeAsync this.ConnectionString "TEST002"
            found.IsSome |> should equal true
            found.Value.AccountType |> should equal AccountType.Liability

            // クリーンアップ
            let! _ = deleteAsync this.ConnectionString "TEST002"
            ()
        }

    [<Fact>]
    member this.``勘定科目種別で検索できる``() =
        task {
            do! this.CleanupTestAccountsAsync()

            // 資産科目を2件登録
            let account1 = Account.create "TEST003" "テスト資産1" AccountType.Asset false
            let account1 = { account1 with Balance = Money.Create(10000m) }
            let account2 = { account1 with AccountCode = AccountCode.Create("TEST004"); AccountName = "テスト資産2" }

            let! _ = insertAsync this.ConnectionString account1
            let! _ = insertAsync this.ConnectionString account2

            let! assets = findByTypeAsync this.ConnectionString "資産"
            assets |> List.filter (fun a -> a.AccountCode.Code.StartsWith("TEST")) |> List.length |> should equal 2

            // クリーンアップ
            let! _ = deleteAsync this.ConnectionString "TEST003"
            let! _ = deleteAsync this.ConnectionString "TEST004"
            ()
        }

    [<Fact>]
    member this.``勘定科目を更新できる``() =
        task {
            do! this.CleanupTestAccountsAsync()

            let account = Account.create "TEST005" "更新前" AccountType.Expense false

            let! _ = insertAsync this.ConnectionString account

            // 更新
            let updated = { account with AccountName = "更新後"; Balance = Money.Create(99999m) }
            let! _ = updateAsync this.ConnectionString updated

            // 確認
            let! found = findByCodeAsync this.ConnectionString "TEST005"
            found.Value.AccountName |> should equal "更新後"
            found.Value.Balance.Amount |> should equal 99999m

            // クリーンアップ
            let! _ = deleteAsync this.ConnectionString "TEST005"
            ()
        }

    [<Fact>]
    member this.``勘定科目を削除できる``() =
        task {
            do! this.CleanupTestAccountsAsync()

            let account = Account.create "TEST006" "削除テスト" AccountType.Revenue false

            let! _ = insertAsync this.ConnectionString account

            // 削除
            let! _ = deleteAsync this.ConnectionString "TEST006"

            // 確認
            let! found = findByCodeAsync this.ConnectionString "TEST006"
            found.IsNone |> should equal true
        }

    [<Fact>]
    member this.``残高を更新できる``() =
        task {
            do! this.CleanupTestAccountsAsync()

            let account = Account.create "TEST007" "残高更新テスト" AccountType.Asset false
            let account = { account with Balance = Money.Create(1000m) }

            let! _ = insertAsync this.ConnectionString account

            // 残高更新
            let! _ = updateBalanceAsync this.ConnectionString "TEST007" 5000m

            // 確認
            let! found = findByCodeAsync this.ConnectionString "TEST007"
            found.Value.Balance.Amount |> should equal 5000m

            // クリーンアップ
            let! _ = deleteAsync this.ConnectionString "TEST007"
            ()
        }

    [<Fact>]
    member this.``勘定科目カナを設定できる``() =
        task {
            do! this.CleanupTestAccountsAsync()

            let account = Account.create "TEST008" "カナテスト科目" AccountType.Asset false
            let account = { account with AccountNameKana = Some "カナテストカモク" }

            let! _ = insertAsync this.ConnectionString account

            let! found = findByCodeAsync this.ConnectionString "TEST008"
            found.IsSome |> should equal true
            found.Value.AccountNameKana |> should equal (Some "カナテストカモク")

            // クリーンアップ
            let! _ = deleteAsync this.ConnectionString "TEST008"
            ()
        }

    [<Fact>]
    member this.``BSPL区分と取引要素区分を設定できる``() =
        task {
            do! this.CleanupTestAccountsAsync()

            let account = Account.create "TEST009" "区分テスト科目" AccountType.Asset false
            // BSPL区分は BalanceSheet または ProfitAndLoss
            // 取引要素区分は AssetElement/LiabilityElement/EquityElement/RevenueElement/ExpenseElement
            let account = { account with
                              BsplType = Some BsplType.BalanceSheet
                              TransactionElementType = Some TransactionElementType.AssetElement }

            let! _ = insertAsync this.ConnectionString account

            let! found = findByCodeAsync this.ConnectionString "TEST009"
            found.IsSome |> should equal true
            found.Value.BsplType |> should equal (Some BsplType.BalanceSheet)
            found.Value.TransactionElementType |> should equal (Some TransactionElementType.AssetElement)

            // クリーンアップ
            let! _ = deleteAsync this.ConnectionString "TEST009"
            ()
        }

    [<Fact>]
    member this.``合計科目と明細科目を取得できる``() =
        task {
            do! this.CleanupTestAccountsAsync()

            // 合計科目を登録
            let summaryAccount = Account.create "TEST010" "合計科目テスト" AccountType.Asset true
            let! _ = insertAsync this.ConnectionString summaryAccount

            // 明細科目を登録
            let detailAccount = Account.create "TEST011" "明細科目テスト" AccountType.Asset false
            let! _ = insertAsync this.ConnectionString detailAccount

            // 合計科目を取得
            let! summaryAccounts = findSummaryAccountsAsync this.ConnectionString
            summaryAccounts |> List.exists (fun a -> a.AccountCode.Code = "TEST010") |> should equal true

            // 明細科目を取得
            let! detailAccounts = findDetailAccountsAsync this.ConnectionString
            detailAccounts |> List.exists (fun a -> a.AccountCode.Code = "TEST011") |> should equal true

            // クリーンアップ
            let! _ = deleteAsync this.ConnectionString "TEST010"
            let! _ = deleteAsync this.ConnectionString "TEST011"
            ()
        }
