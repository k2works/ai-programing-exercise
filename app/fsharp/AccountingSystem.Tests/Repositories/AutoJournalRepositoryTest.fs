module AccountingSystem.Tests.Repositories.AutoJournalRepositoryTest

open System
open Xunit
open FsUnit.Xunit
open Npgsql
open AccountingSystem.Domain.Models.AutoJournalPattern
open AccountingSystem.Domain.Types
open AccountingSystem.Application.Repositories
open AccountingSystem.Infrastructure.Repositories
open AccountingSystem.Tests.DatabaseTestBase

/// <summary>
/// AutoJournalRepository の統合テスト
/// Testcontainers で PostgreSQL を使用
/// </summary>
type AutoJournalRepositoryTest() =
    inherit DatabaseTestBase()

    /// テストデータをクリーンアップ
    member private this.CleanupTestDataAsync() =
        task {
            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()

            use cmd = new NpgsqlCommand("""
                DELETE FROM "自動仕訳実行ログ";
                DELETE FROM "自動仕訳パターン明細";
                DELETE FROM "自動仕訳パターン";
            """, conn)
            let! _ = cmd.ExecuteNonQueryAsync()
            ()
        }

    /// テスト用パターンを作成
    member private _.CreateTestPattern(code: string, name: string, isActive: bool) : AutoJournalPattern =
        {
            Id = 0L
            PatternCode = code
            PatternName = name
            SourceTableName = "テストテーブル"
            Description = Some "テスト説明"
            IsActive = isActive
        }

    /// テスト用パターン明細を作成
    member private _.CreateTestPatternItem(patternId: int64, lineNo: int, dcType: DebitCreditType, accountCode: string) : AutoJournalPatternItem =
        {
            Id = 0L
            PatternId = patternId
            LineNumber = lineNo
            DebitCreditType = dcType
            AccountCode = AccountCode.Create(accountCode)
            AmountExpression = "{金額}"
            DescriptionTemplate = Some "テスト摘要"
        }

    [<Fact>]
    member this.``SavePatternAsync で新規パターンを保存できる``() =
        task {
            // Given
            do! this.CleanupTestDataAsync()
            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()

            let repository = AutoJournalRepository(conn) :> IAutoJournalRepository
            let pattern = this.CreateTestPattern("TEST001", "テストパターン", true)

            // When
            let! saved = repository.SavePatternAsync(pattern)

            // Then
            saved.Id |> should be (greaterThan 0L)
            saved.PatternCode |> should equal "TEST001"
            saved.PatternName |> should equal "テストパターン"
            saved.IsActive |> should equal true
        }

    [<Fact>]
    member this.``SavePatternAsync で既存パターンを更新できる``() =
        task {
            // Given
            do! this.CleanupTestDataAsync()
            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()

            let repository = AutoJournalRepository(conn) :> IAutoJournalRepository
            let pattern = this.CreateTestPattern("TEST001", "テストパターン", true)
            let! saved = repository.SavePatternAsync(pattern)

            // When
            let updated = { saved with PatternName = "更新後パターン"; IsActive = false }
            let! result = repository.SavePatternAsync(updated)

            // Then
            result.Id |> should equal saved.Id
            result.PatternName |> should equal "更新後パターン"
            result.IsActive |> should equal false
        }

    [<Fact>]
    member this.``GetPatternByCodeAsync で存在するパターンを取得できる``() =
        task {
            // Given
            do! this.CleanupTestDataAsync()
            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()

            let repository = AutoJournalRepository(conn) :> IAutoJournalRepository
            let pattern = this.CreateTestPattern("TEST001", "テストパターン", true)
            let! _ = repository.SavePatternAsync(pattern)

            // When
            let! result = repository.GetPatternByCodeAsync("TEST001")

            // Then
            result.IsSome |> should equal true
            result.Value.PatternCode |> should equal "TEST001"
            result.Value.PatternName |> should equal "テストパターン"
        }

    [<Fact>]
    member this.``GetPatternByCodeAsync で存在しないパターンは None を返す``() =
        task {
            // Given
            do! this.CleanupTestDataAsync()
            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()

            let repository = AutoJournalRepository(conn) :> IAutoJournalRepository

            // When
            let! result = repository.GetPatternByCodeAsync("NOTEXIST")

            // Then
            result.IsNone |> should equal true
        }

    [<Fact>]
    member this.``GetPatternsAsync で有効なパターンのみ取得できる``() =
        task {
            // Given
            do! this.CleanupTestDataAsync()
            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()

            let repository = AutoJournalRepository(conn) :> IAutoJournalRepository
            let! _ = repository.SavePatternAsync(this.CreateTestPattern("ACTIVE1", "有効1", true))
            let! _ = repository.SavePatternAsync(this.CreateTestPattern("INACTIVE1", "無効1", false))
            let! _ = repository.SavePatternAsync(this.CreateTestPattern("ACTIVE2", "有効2", true))

            // When
            let query: AutoJournalPatternQuery = { PatternCode = None; IsActive = Some true }
            let! patterns = repository.GetPatternsAsync(query)

            // Then
            patterns.Length |> should equal 2
            patterns |> List.forall (fun p -> p.IsActive) |> should equal true
        }

    [<Fact>]
    member this.``GetPatternsAsync で全パターンを取得できる``() =
        task {
            // Given
            do! this.CleanupTestDataAsync()
            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()

            let repository = AutoJournalRepository(conn) :> IAutoJournalRepository
            let! _ = repository.SavePatternAsync(this.CreateTestPattern("TEST1", "テスト1", true))
            let! _ = repository.SavePatternAsync(this.CreateTestPattern("TEST2", "テスト2", false))

            // When
            let query: AutoJournalPatternQuery = { PatternCode = None; IsActive = None }
            let! patterns = repository.GetPatternsAsync(query)

            // Then
            patterns.Length |> should equal 2
        }

    [<Fact>]
    member this.``SavePatternItemsAsync でパターン明細を保存できる``() =
        task {
            // Given
            do! this.CleanupTestDataAsync()
            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()

            let repository = AutoJournalRepository(conn) :> IAutoJournalRepository
            let! pattern = repository.SavePatternAsync(this.CreateTestPattern("TEST001", "テスト", true))

            let items = [
                this.CreateTestPatternItem(pattern.Id, 1, Debit, "130")
                this.CreateTestPatternItem(pattern.Id, 2, Credit, "500")
            ]

            // When
            do! repository.SavePatternItemsAsync pattern.Id items

            // Then
            let! savedItems = repository.GetPatternItemsAsync(pattern.Id)
            savedItems.Length |> should equal 2
            savedItems.[0].LineNumber |> should equal 1
            savedItems.[0].DebitCreditType |> should equal Debit
            savedItems.[1].LineNumber |> should equal 2
            savedItems.[1].DebitCreditType |> should equal Credit
        }

    [<Fact>]
    member this.``SavePatternItemsAsync で既存明細を置き換える``() =
        task {
            // Given
            do! this.CleanupTestDataAsync()
            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()

            let repository = AutoJournalRepository(conn) :> IAutoJournalRepository
            let! pattern = repository.SavePatternAsync(this.CreateTestPattern("TEST001", "テスト", true))

            // 初回の明細保存
            let items1 = [
                this.CreateTestPatternItem(pattern.Id, 1, Debit, "130")
                this.CreateTestPatternItem(pattern.Id, 2, Credit, "500")
            ]
            do! repository.SavePatternItemsAsync pattern.Id items1

            // When: 異なる明細で上書き
            let items2 = [
                this.CreateTestPatternItem(pattern.Id, 1, Debit, "111")
            ]
            do! repository.SavePatternItemsAsync pattern.Id items2

            // Then
            let! savedItems = repository.GetPatternItemsAsync(pattern.Id)
            savedItems.Length |> should equal 1
            savedItems.[0].AccountCode.Code |> should equal "111"
        }

    [<Fact>]
    member this.``GetPatternItemsAsync でパターン明細を取得できる``() =
        task {
            // Given
            do! this.CleanupTestDataAsync()
            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()

            let repository = AutoJournalRepository(conn) :> IAutoJournalRepository
            let! pattern = repository.SavePatternAsync(this.CreateTestPattern("TEST001", "テスト", true))

            let item1: AutoJournalPatternItem = {
                Id = 0L
                PatternId = pattern.Id
                LineNumber = 1
                DebitCreditType = Debit
                AccountCode = AccountCode.Create("130")
                AmountExpression = "{売上金額}"
                DescriptionTemplate = Some "売上計上"
            }
            let item2: AutoJournalPatternItem = {
                Id = 0L
                PatternId = pattern.Id
                LineNumber = 2
                DebitCreditType = Credit
                AccountCode = AccountCode.Create("500")
                AmountExpression = "{売上金額}"
                DescriptionTemplate = None
            }
            do! repository.SavePatternItemsAsync pattern.Id [item1; item2]

            // When
            let! savedItems = repository.GetPatternItemsAsync(pattern.Id)

            // Then
            savedItems.Length |> should equal 2
            savedItems.[0].AmountExpression |> should equal "{売上金額}"
            savedItems.[0].DescriptionTemplate |> should equal (Some "売上計上")
            savedItems.[1].DescriptionTemplate |> should equal None
        }

    [<Fact>]
    member this.``SaveLogAsync で実行ログを保存できる``() =
        task {
            // Given
            do! this.CleanupTestDataAsync()
            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()

            let repository = AutoJournalRepository(conn) :> IAutoJournalRepository
            let! pattern = repository.SavePatternAsync(this.CreateTestPattern("TEST001", "テスト", true))

            let log = AutoJournalLog.createSuccess pattern.Id 100 100

            // When
            let! saved = repository.SaveLogAsync(log)

            // Then
            saved.Id |> should be (greaterThan 0L)
            saved.PatternId |> should equal pattern.Id
            saved.ProcessedCount |> should equal 100
            saved.GeneratedCount |> should equal 100
            saved.Status |> should equal Success
        }

    [<Fact>]
    member this.``SaveLogAsync で失敗ログを保存できる``() =
        task {
            // Given
            do! this.CleanupTestDataAsync()
            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()

            let repository = AutoJournalRepository(conn) :> IAutoJournalRepository
            let! pattern = repository.SavePatternAsync(this.CreateTestPattern("TEST001", "テスト", true))

            let log = AutoJournalLog.createFailure pattern.Id 50 "処理エラー" "詳細なエラー内容"

            // When
            let! saved = repository.SaveLogAsync(log)

            // Then
            saved.Status |> should equal Failure
            saved.Message |> should equal (Some "処理エラー")
            saved.ErrorDetail |> should equal (Some "詳細なエラー内容")
        }

    [<Fact>]
    member this.``GetLogsByPatternIdAsync で実行ログを取得できる``() =
        task {
            // Given
            do! this.CleanupTestDataAsync()
            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()

            let repository = AutoJournalRepository(conn) :> IAutoJournalRepository
            let! pattern = repository.SavePatternAsync(this.CreateTestPattern("TEST001", "テスト", true))

            // 3件のログを保存
            let! _ = repository.SaveLogAsync(AutoJournalLog.createSuccess pattern.Id 10 10)
            let! _ = repository.SaveLogAsync(AutoJournalLog.createSuccess pattern.Id 20 20)
            let! _ = repository.SaveLogAsync(AutoJournalLog.createSuccess pattern.Id 30 30)

            // When
            let! logs = repository.GetLogsByPatternIdAsync pattern.Id 2

            // Then
            logs.Length |> should equal 2
        }

    [<Fact>]
    member this.``GetLogsByPatternIdAsync でログが実行日時の降順で返される``() =
        task {
            // Given
            do! this.CleanupTestDataAsync()
            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()

            let repository = AutoJournalRepository(conn) :> IAutoJournalRepository
            let! pattern = repository.SavePatternAsync(this.CreateTestPattern("TEST001", "テスト", true))

            // 異なる処理件数でログを保存（挿入順で時刻が異なる）
            let! _ = repository.SaveLogAsync(AutoJournalLog.createSuccess pattern.Id 10 10)
            let! _ = repository.SaveLogAsync(AutoJournalLog.createSuccess pattern.Id 20 20)
            let! _ = repository.SaveLogAsync(AutoJournalLog.createSuccess pattern.Id 30 30)

            // When
            let! logs = repository.GetLogsByPatternIdAsync pattern.Id 10

            // Then
            logs.Length |> should equal 3
            // 最新のログ（処理件数30）が最初に来る
            logs.[0].ProcessedCount |> should equal 30
        }
