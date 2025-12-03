module AccountingSystem.Tests.AccountTest

open Xunit
open FsUnit.Xunit
open Npgsql
open System
open AccountingSystem.Tests.DatabaseTestBase

/// <summary>
/// 勘定科目マスタのテスト
///
/// このテストでは、勘定科目マスタに対するCRUD操作を検証します。
/// Testcontainersを使用して、実際のPostgreSQLコンテナでテストを実行します。
/// </summary>
type AccountTest() =
    inherit DatabaseTestBase()

    /// <summary>
    /// テーブルのデータをクリアするヘルパーメソッド
    /// </summary>
    member private this.CleanupAsync() =
        task {
            use connection = new NpgsqlConnection(this.ConnectionString)
            do! connection.OpenAsync()
            use cmd = new NpgsqlCommand(@"TRUNCATE TABLE ""勘定科目マスタ"" CASCADE", connection)
            let! _ = cmd.ExecuteNonQueryAsync()
            return ()
        }

    /// <summary>
    /// 勘定科目を登録するヘルパーメソッド
    /// </summary>
    member private this.InsertAccountAsync(code: string, name: string, accountType: string, balance: decimal) =
        task {
            use connection = new NpgsqlConnection(this.ConnectionString)
            do! connection.OpenAsync()

            let sql = @"
                INSERT INTO ""勘定科目マスタ"" (""勘定科目コード"", ""勘定科目名"", ""勘定科目種別"", ""残高"")
                VALUES (@code, @name, @type::account_type, @balance)
                RETURNING ""勘定科目ID""
            "

            use cmd = new NpgsqlCommand(sql, connection)
            cmd.Parameters.AddWithValue("code", code) |> ignore
            cmd.Parameters.AddWithValue("name", name) |> ignore
            cmd.Parameters.AddWithValue("type", accountType) |> ignore
            cmd.Parameters.AddWithValue("balance", balance) |> ignore

            let! result = cmd.ExecuteScalarAsync()
            return Convert.ToInt32(result)
        }

    [<Fact>]
    member this.``勘定科目を登録できる``() =
        task {
            // 各テスト前にデータをクリア
            do! this.CleanupAsync()

            use connection = new NpgsqlConnection(this.ConnectionString)
            do! connection.OpenAsync()

            // 1. テストデータを作成
            let sql = @"
                INSERT INTO ""勘定科目マスタ"" (""勘定科目コード"", ""勘定科目名"", ""勘定科目種別"", ""残高"")
                VALUES (@code, @name, @type::account_type, @balance)
                RETURNING ""勘定科目ID"", ""勘定科目コード"", ""勘定科目名"", ""勘定科目種別""::text, ""残高""
            "

            use cmd = new NpgsqlCommand(sql, connection)
            cmd.Parameters.AddWithValue("code", "1000") |> ignore
            cmd.Parameters.AddWithValue("name", "現金") |> ignore
            cmd.Parameters.AddWithValue("type", "資産") |> ignore
            cmd.Parameters.AddWithValue("balance", 50000.00m) |> ignore

            use! reader = cmd.ExecuteReaderAsync()

            // 2. 取得したデータが期待通りか検証
            let! hasData = reader.ReadAsync()
            hasData |> should equal true

            reader.GetString(reader.GetOrdinal("勘定科目コード")) |> should equal "1000"
            reader.GetString(reader.GetOrdinal("勘定科目名")) |> should equal "現金"
            reader.GetString(reader.GetOrdinal("勘定科目種別")) |> should equal "資産"
            reader.GetDecimal(reader.GetOrdinal("残高")) |> should equal 50000.00m
        }

    [<Fact>]
    member this.``すべての勘定科目を取得できる``() =
        task {
            do! this.CleanupAsync()

            // 1. 複数の勘定科目を登録
            let! _ = this.InsertAccountAsync("1000", "現金", "資産", 50000.00m)
            let! _ = this.InsertAccountAsync("2000", "買掛金", "負債", 30000.00m)
            let! _ = this.InsertAccountAsync("3000", "資本金", "純資産", 100000.00m)

            use connection = new NpgsqlConnection(this.ConnectionString)
            do! connection.OpenAsync()

            // 2. すべての勘定科目を取得
            let sql = @"
                SELECT ""勘定科目コード"", ""勘定科目名"", ""勘定科目種別"", ""残高""
                FROM ""勘定科目マスタ""
                ORDER BY ""勘定科目コード""
            "

            let codes = ResizeArray<string>()
            use cmd = new NpgsqlCommand(sql, connection)
            use! reader = cmd.ExecuteReaderAsync()

            while reader.Read() do
                codes.Add(reader.GetString(reader.GetOrdinal("勘定科目コード")))

            // 3. 期待通りのデータが取得できるか検証
            codes.Count |> should equal 3
            codes |> should contain "1000"
            codes |> should contain "2000"
            codes |> should contain "3000"
        }

    [<Fact>]
    member this.``勘定科目コードで検索できる``() =
        task {
            do! this.CleanupAsync()

            // 1. テストデータを登録
            let! _ = this.InsertAccountAsync("1000", "現金", "資産", 50000.00m)

            use connection = new NpgsqlConnection(this.ConnectionString)
            do! connection.OpenAsync()

            // 2. コードで検索
            let sql = @"
                SELECT ""勘定科目コード"", ""勘定科目名"", ""勘定科目種別""::text
                FROM ""勘定科目マスタ""
                WHERE ""勘定科目コード"" = @code
            "

            use cmd = new NpgsqlCommand(sql, connection)
            cmd.Parameters.AddWithValue("code", "1000") |> ignore
            use! reader = cmd.ExecuteReaderAsync()

            // 3. 正しいデータが取得できるか検証
            let! hasData = reader.ReadAsync()
            hasData |> should equal true
            reader.GetString(reader.GetOrdinal("勘定科目名")) |> should equal "現金"
            reader.GetString(reader.GetOrdinal("勘定科目種別")) |> should equal "資産"
        }

    [<Fact>]
    member this.``勘定科目を更新できる``() =
        task {
            do! this.CleanupAsync()

            // 1. データを登録
            let! accountId = this.InsertAccountAsync("1000", "現金", "資産", 50000.00m)

            use connection = new NpgsqlConnection(this.ConnectionString)
            do! connection.OpenAsync()

            // 2. データを更新
            let updateSql = @"
                UPDATE ""勘定科目マスタ""
                SET ""勘定科目名"" = @name, ""残高"" = @balance, ""更新日時"" = CURRENT_TIMESTAMP
                WHERE ""勘定科目ID"" = @id
            "

            use updateCmd = new NpgsqlCommand(updateSql, connection)
            updateCmd.Parameters.AddWithValue("name", "現金及び預金") |> ignore
            updateCmd.Parameters.AddWithValue("balance", 75000.00m) |> ignore
            updateCmd.Parameters.AddWithValue("id", accountId) |> ignore
            let! updated = updateCmd.ExecuteNonQueryAsync()
            updated |> should equal 1

            // 3. 更新されたか検証
            let selectSql = @"
                SELECT ""勘定科目コード"", ""勘定科目名"", ""残高""
                FROM ""勘定科目マスタ""
                WHERE ""勘定科目ID"" = @id
            "

            use selectCmd = new NpgsqlCommand(selectSql, connection)
            selectCmd.Parameters.AddWithValue("id", accountId) |> ignore
            use! reader = selectCmd.ExecuteReaderAsync()

            let! hasData = reader.ReadAsync()
            hasData |> should equal true
            reader.GetString(reader.GetOrdinal("勘定科目名")) |> should equal "現金及び預金"
            reader.GetDecimal(reader.GetOrdinal("残高")) |> should equal 75000.00m
            reader.GetString(reader.GetOrdinal("勘定科目コード")) |> should equal "1000"
        }

    [<Fact>]
    member this.``勘定科目を削除できる``() =
        task {
            do! this.CleanupAsync()

            // 1. データを登録
            let! accountId = this.InsertAccountAsync("1000", "現金", "資産", 50000.00m)

            use connection = new NpgsqlConnection(this.ConnectionString)
            do! connection.OpenAsync()

            // 2. データを削除
            let deleteSql = @"
                DELETE FROM ""勘定科目マスタ""
                WHERE ""勘定科目ID"" = @id
            "

            use deleteCmd = new NpgsqlCommand(deleteSql, connection)
            deleteCmd.Parameters.AddWithValue("id", accountId) |> ignore
            let! deleted = deleteCmd.ExecuteNonQueryAsync()
            deleted |> should equal 1

            // 3. データが削除されたか検証
            let selectSql = @"
                SELECT COUNT(*) as count
                FROM ""勘定科目マスタ""
                WHERE ""勘定科目ID"" = @id
            "

            use selectCmd = new NpgsqlCommand(selectSql, connection)
            selectCmd.Parameters.AddWithValue("id", accountId) |> ignore
            let! count = selectCmd.ExecuteScalarAsync()
            Convert.ToInt64(count) |> should equal 0L
        }

    [<Fact>]
    member this.``勘定科目種別でフィルタリングできる``() =
        task {
            do! this.CleanupAsync()

            // 1. 複数の勘定科目を登録
            let! _ = this.InsertAccountAsync("1000", "現金", "資産", 50000.00m)
            let! _ = this.InsertAccountAsync("2000", "買掛金", "負債", 30000.00m)
            let! _ = this.InsertAccountAsync("3000", "資本金", "純資産", 100000.00m)

            use connection = new NpgsqlConnection(this.ConnectionString)
            do! connection.OpenAsync()

            // 2. 資産勘定のみを取得
            let sql = @"
                SELECT ""勘定科目名""
                FROM ""勘定科目マスタ""
                WHERE ""勘定科目種別"" = @type::account_type
            "

            let assetNames = ResizeArray<string>()
            use assetCmd = new NpgsqlCommand(sql, connection)
            assetCmd.Parameters.AddWithValue("type", "資産") |> ignore
            use! assetReader = assetCmd.ExecuteReaderAsync()
            while assetReader.Read() do
                assetNames.Add(assetReader.GetString(0))

            // 3. 正しくフィルタリングされるか検証
            assetNames.Count |> should equal 1
            assetNames |> should contain "現金"
        }
