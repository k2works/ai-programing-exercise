module AccountingSystem.Tests.Infrastructure.BalanceRepositoryTest

open System
open Xunit
open FsUnit.Xunit
open Npgsql
open AccountingSystem.Tests.DatabaseTestBase
open AccountingSystem.Application.Port.Out
open AccountingSystem.Infrastructure.Adapters

/// <summary>
/// BalanceRepository のテスト
/// </summary>
type BalanceRepositoryTest() =
    inherit DatabaseTestBase()

    /// リポジトリを取得
    member private this.CreateRepository() : IBalanceRepository =
        BalanceRepositoryAdapter(this.ConnectionString)

    /// テスト用の勘定科目をセットアップ
    member private this.SetupTestAccountsAsync() =
        task {
            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()

            // テスト用勘定科目を登録（存在しない場合のみ）
            use cmd = new NpgsqlCommand("""
                INSERT INTO "勘定科目マスタ" ("勘定科目コード", "勘定科目名", "勘定科目種別", "残高")
                VALUES ('1020', '普通預金', '資産', 0)
                ON CONFLICT ("勘定科目コード") DO NOTHING;

                INSERT INTO "勘定科目マスタ" ("勘定科目コード", "勘定科目名", "勘定科目種別", "残高")
                VALUES ('1130', '売掛金', '資産', 0)
                ON CONFLICT ("勘定科目コード") DO NOTHING;

                INSERT INTO "勘定科目マスタ" ("勘定科目コード", "勘定科目名", "勘定科目種別", "残高")
                VALUES ('4010', '売上高', '収益', 0)
                ON CONFLICT ("勘定科目コード") DO NOTHING;

                INSERT INTO "勘定科目マスタ" ("勘定科目コード", "勘定科目名", "勘定科目種別", "残高")
                VALUES ('5010', '売上原価', '費用', 0)
                ON CONFLICT ("勘定科目コード") DO NOTHING;
            """, conn)
            let! _ = cmd.ExecuteNonQueryAsync()
            ()
        }

    member private this.CleanupAsync() =
        task {
            do! this.SetupTestAccountsAsync()
            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()
            use cmd = new NpgsqlCommand("""DELETE FROM "日次勘定科目残高" """, conn)
            let! _ = cmd.ExecuteNonQueryAsync()
            ()
        }

    [<Fact>]
    member this.``UpdateDailyBalanceAsync で日次残高レコードを登録できる``() =
        task {
            // Given: 2025-01-15 の普通預金の日次残高
            let entryDate = DateTime(2025, 1, 15)
            let accountCode = "1020"  // 普通預金

            do! this.CleanupAsync()
            let repository = this.CreateRepository()

            // When: 日次残高を登録
            let param: DailyBalanceUpdateParams = {
                EntryDate = entryDate
                AccountCode = accountCode
                SubAccountCode = None
                DepartmentCode = None
                ProjectCode = None
                SettlementFlag = None
                DebitAmount = 100000.00M
                CreditAmount = 0.00M
            }
            do! repository.UpdateDailyBalanceAsync(param)

            // Then: データが正しく登録されている
            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()

            use selectCmd = new NpgsqlCommand("""
                SELECT * FROM "日次勘定科目残高"
                WHERE "起票日" = @EntryDate
                  AND "勘定科目コード" = @AccountCode
                """, conn)
            selectCmd.Parameters.AddWithValue("EntryDate", entryDate) |> ignore
            selectCmd.Parameters.AddWithValue("AccountCode", accountCode) |> ignore

            use! reader = selectCmd.ExecuteReaderAsync()
            let! hasRow = reader.ReadAsync()

            hasRow |> should equal true
            reader.GetDecimal(reader.GetOrdinal("借方金額")) |> should equal 100000.00M
            reader.GetDecimal(reader.GetOrdinal("貸方金額")) |> should equal 0.00M
        }

    [<Fact>]
    member this.``UpdateDailyBalanceAsync で同じキーの場合は金額が加算される（UPSERT）``() =
        task {
            // Given: 同じキーで日次残高を2回登録
            let entryDate = DateTime(2025, 1, 15)
            let accountCode = "1020"

            do! this.CleanupAsync()
            let repository = this.CreateRepository()

            // When: 1回目の登録
            let param1: DailyBalanceUpdateParams = {
                EntryDate = entryDate
                AccountCode = accountCode
                SubAccountCode = None
                DepartmentCode = None
                ProjectCode = None
                SettlementFlag = None
                DebitAmount = 100000.00M
                CreditAmount = 0.00M
            }
            do! repository.UpdateDailyBalanceAsync(param1)

            // 2回目の登録（同じキー）
            let param2: DailyBalanceUpdateParams = {
                EntryDate = entryDate
                AccountCode = accountCode
                SubAccountCode = None
                DepartmentCode = None
                ProjectCode = None
                SettlementFlag = None
                DebitAmount = 50000.00M
                CreditAmount = 0.00M
            }
            do! repository.UpdateDailyBalanceAsync(param2)

            // Then: 金額が加算されている
            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()

            use selectCmd = new NpgsqlCommand("""
                SELECT "借方金額", "貸方金額" FROM "日次勘定科目残高"
                WHERE "起票日" = @EntryDate
                  AND "勘定科目コード" = @AccountCode
                """, conn)
            selectCmd.Parameters.AddWithValue("EntryDate", entryDate) |> ignore
            selectCmd.Parameters.AddWithValue("AccountCode", accountCode) |> ignore

            use! reader = selectCmd.ExecuteReaderAsync()
            let! hasRow = reader.ReadAsync()

            hasRow |> should equal true
            reader.GetDecimal(0) |> should equal 150000.00M  // 100000 + 50000
            reader.GetDecimal(1) |> should equal 0.00M
        }

    [<Fact>]
    member this.``UpdateDailyBalanceAsync で部門別の残高を管理できる``() =
        task {
            // Given: 売上高の部門別日次残高
            let entryDate = DateTime(2025, 1, 15)
            let accountCode = "4010"  // 売上高

            do! this.CleanupAsync()
            let repository = this.CreateRepository()

            // When: 部門001と部門002の残高を登録
            let param1: DailyBalanceUpdateParams = {
                EntryDate = entryDate
                AccountCode = accountCode
                SubAccountCode = None
                DepartmentCode = Some "001"
                ProjectCode = None
                SettlementFlag = None
                DebitAmount = 0.00M
                CreditAmount = 300000.00M
            }
            do! repository.UpdateDailyBalanceAsync(param1)

            let param2: DailyBalanceUpdateParams = {
                EntryDate = entryDate
                AccountCode = accountCode
                SubAccountCode = None
                DepartmentCode = Some "002"
                ProjectCode = None
                SettlementFlag = None
                DebitAmount = 0.00M
                CreditAmount = 200000.00M
            }
            do! repository.UpdateDailyBalanceAsync(param2)

            // Then: 部門別に集計できる
            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()

            use selectCmd = new NpgsqlCommand("""
                SELECT "部門コード", SUM("貸方金額") as 売上合計
                FROM "日次勘定科目残高"
                WHERE "勘定科目コード" = @AccountCode
                GROUP BY "部門コード"
                ORDER BY "部門コード"
                """, conn)
            selectCmd.Parameters.AddWithValue("AccountCode", accountCode) |> ignore

            use! reader = selectCmd.ExecuteReaderAsync()

            let! hasRow1 = reader.ReadAsync()
            hasRow1 |> should equal true
            reader.GetString(0) |> should equal "001"
            reader.GetDecimal(1) |> should equal 300000.00M

            let! hasRow2 = reader.ReadAsync()
            hasRow2 |> should equal true
            reader.GetString(0) |> should equal "002"
            reader.GetDecimal(1) |> should equal 200000.00M
        }

    [<Fact>]
    member this.``UpdateDailyBalanceAsync でプロジェクト別の残高を管理できる``() =
        task {
            // Given: プロジェクト別の残高
            let entryDate = DateTime(2025, 1, 15)
            let accountCode = "4010"  // 売上高

            do! this.CleanupAsync()
            let repository = this.CreateRepository()

            // When: プロジェクトP001とP002の残高を登録
            let param1: DailyBalanceUpdateParams = {
                EntryDate = entryDate
                AccountCode = accountCode
                SubAccountCode = None
                DepartmentCode = None
                ProjectCode = Some "P001"
                SettlementFlag = None
                DebitAmount = 0.00M
                CreditAmount = 150000.00M
            }
            do! repository.UpdateDailyBalanceAsync(param1)

            let param2: DailyBalanceUpdateParams = {
                EntryDate = entryDate
                AccountCode = accountCode
                SubAccountCode = None
                DepartmentCode = None
                ProjectCode = Some "P002"
                SettlementFlag = None
                DebitAmount = 0.00M
                CreditAmount = 250000.00M
            }
            do! repository.UpdateDailyBalanceAsync(param2)

            // Then: プロジェクト別に集計できる
            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()

            use selectCmd = new NpgsqlCommand("""
                SELECT "プロジェクトコード", SUM("貸方金額") as 売上合計
                FROM "日次勘定科目残高"
                WHERE "勘定科目コード" = @AccountCode
                GROUP BY "プロジェクトコード"
                ORDER BY "プロジェクトコード"
                """, conn)
            selectCmd.Parameters.AddWithValue("AccountCode", accountCode) |> ignore

            use! reader = selectCmd.ExecuteReaderAsync()

            let! hasRow1 = reader.ReadAsync()
            hasRow1 |> should equal true
            reader.GetString(0) |> should equal "P001"
            reader.GetDecimal(1) |> should equal 150000.00M

            let! hasRow2 = reader.ReadAsync()
            hasRow2 |> should equal true
            reader.GetString(0) |> should equal "P002"
            reader.GetDecimal(1) |> should equal 250000.00M
        }

    [<Fact>]
    member this.``UpdateDailyBalanceAsync で補助科目別の残高を管理できる``() =
        task {
            // Given: 売掛金の補助科目（得意先）別残高
            let entryDate = DateTime(2025, 1, 15)
            let accountCode = "1130"  // 売掛金

            do! this.CleanupAsync()
            let repository = this.CreateRepository()

            // When: 得意先A001とA002の残高を登録
            let param1: DailyBalanceUpdateParams = {
                EntryDate = entryDate
                AccountCode = accountCode
                SubAccountCode = Some "A001"
                DepartmentCode = None
                ProjectCode = None
                SettlementFlag = None
                DebitAmount = 500000.00M
                CreditAmount = 0.00M
            }
            do! repository.UpdateDailyBalanceAsync(param1)

            let param2: DailyBalanceUpdateParams = {
                EntryDate = entryDate
                AccountCode = accountCode
                SubAccountCode = Some "A002"
                DepartmentCode = None
                ProjectCode = None
                SettlementFlag = None
                DebitAmount = 300000.00M
                CreditAmount = 0.00M
            }
            do! repository.UpdateDailyBalanceAsync(param2)

            // Then: 補助科目別に集計できる
            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()

            use selectCmd = new NpgsqlCommand("""
                SELECT "補助科目コード", SUM("借方金額") as 売掛金合計
                FROM "日次勘定科目残高"
                WHERE "勘定科目コード" = @AccountCode
                GROUP BY "補助科目コード"
                ORDER BY "補助科目コード"
                """, conn)
            selectCmd.Parameters.AddWithValue("AccountCode", accountCode) |> ignore

            use! reader = selectCmd.ExecuteReaderAsync()

            let! hasRow1 = reader.ReadAsync()
            hasRow1 |> should equal true
            reader.GetString(0) |> should equal "A001"
            reader.GetDecimal(1) |> should equal 500000.00M

            let! hasRow2 = reader.ReadAsync()
            hasRow2 |> should equal true
            reader.GetString(0) |> should equal "A002"
            reader.GetDecimal(1) |> should equal 300000.00M
        }

    [<Fact>]
    member this.``UpdateDailyBalanceAsync で決算仕訳フラグで通常仕訳と決算仕訳を分けて管理できる``() =
        task {
            // Given: 通常仕訳と決算仕訳の残高
            let entryDate = DateTime(2025, 3, 31)
            let accountCode = "5010"  // 売上原価

            do! this.CleanupAsync()
            let repository = this.CreateRepository()

            // When: 通常仕訳（決算仕訳フラグ=0）の残高を登録
            let param1: DailyBalanceUpdateParams = {
                EntryDate = entryDate
                AccountCode = accountCode
                SubAccountCode = None
                DepartmentCode = None
                ProjectCode = None
                SettlementFlag = Some 0
                DebitAmount = 800000.00M
                CreditAmount = 0.00M
            }
            do! repository.UpdateDailyBalanceAsync(param1)

            // 決算仕訳（決算仕訳フラグ=1）の残高を登録
            let param2: DailyBalanceUpdateParams = {
                EntryDate = entryDate
                AccountCode = accountCode
                SubAccountCode = None
                DepartmentCode = None
                ProjectCode = None
                SettlementFlag = Some 1
                DebitAmount = 0.00M
                CreditAmount = 800000.00M
            }
            do! repository.UpdateDailyBalanceAsync(param2)

            // Then: 決算仕訳フラグで分けて集計できる
            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()

            use selectCmd = new NpgsqlCommand("""
                SELECT "決算仕訳フラグ", SUM("借方金額") as 借方合計, SUM("貸方金額") as 貸方合計
                FROM "日次勘定科目残高"
                WHERE "勘定科目コード" = @AccountCode
                GROUP BY "決算仕訳フラグ"
                ORDER BY "決算仕訳フラグ"
                """, conn)
            selectCmd.Parameters.AddWithValue("AccountCode", accountCode) |> ignore

            use! reader = selectCmd.ExecuteReaderAsync()

            // 通常仕訳
            let! hasRow1 = reader.ReadAsync()
            hasRow1 |> should equal true
            reader.GetInt32(0) |> should equal 0
            reader.GetDecimal(1) |> should equal 800000.00M
            reader.GetDecimal(2) |> should equal 0.00M

            // 決算仕訳
            let! hasRow2 = reader.ReadAsync()
            hasRow2 |> should equal true
            reader.GetInt32(0) |> should equal 1
            reader.GetDecimal(1) |> should equal 0.00M
            reader.GetDecimal(2) |> should equal 800000.00M
        }
