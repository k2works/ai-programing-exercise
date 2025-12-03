module AccountingSystem.Tests.TaxTransactionTest

open Xunit
open FsUnit.Xunit
open Npgsql
open System
open AccountingSystem.Tests.DatabaseTestBase

/// <summary>
/// 課税取引マスタのテスト
/// </summary>
type TaxTransactionTest() =
    inherit DatabaseTestBase()

    [<Fact>]
    member this.``課税取引マスタの初期データが登録されている``() =
        task {
            use connection = new NpgsqlConnection(this.ConnectionString)
            do! connection.OpenAsync()

            let sql = @"
                SELECT COUNT(*) FROM ""課税取引マスタ""
            "

            use cmd = new NpgsqlCommand(sql, connection)
            let! count = cmd.ExecuteScalarAsync()

            Convert.ToInt64(count) |> should be (greaterThanOrEqualTo 8L)
        }

    [<Fact>]
    member this.``標準税率10%の課税取引が存在する``() =
        task {
            use connection = new NpgsqlConnection(this.ConnectionString)
            do! connection.OpenAsync()

            let sql = @"
                SELECT ""課税取引コード"", ""課税取引名"", ""税率""
                FROM ""課税取引マスタ""
                WHERE ""課税取引コード"" = '01'
            "

            use cmd = new NpgsqlCommand(sql, connection)
            use! reader = cmd.ExecuteReaderAsync()

            let! hasData = reader.ReadAsync()
            hasData |> should equal true
            reader.GetString(reader.GetOrdinal("課税取引名")) |> should equal "課税売上10%"
            reader.GetDecimal(reader.GetOrdinal("税率")) |> should equal 10.00m
        }

    [<Fact>]
    member this.``軽減税率8%の課税取引が存在する``() =
        task {
            use connection = new NpgsqlConnection(this.ConnectionString)
            do! connection.OpenAsync()

            let sql = @"
                SELECT ""課税取引コード"", ""課税取引名"", ""税率""
                FROM ""課税取引マスタ""
                WHERE ""課税取引コード"" = '02'
            "

            use cmd = new NpgsqlCommand(sql, connection)
            use! reader = cmd.ExecuteReaderAsync()

            let! hasData = reader.ReadAsync()
            hasData |> should equal true
            reader.GetString(reader.GetOrdinal("課税取引名")) |> should equal "課税売上8%（軽減）"
            reader.GetDecimal(reader.GetOrdinal("税率")) |> should equal 8.00m
        }

    [<Fact>]
    member this.``勘定科目に課税取引コードを設定できる``() =
        task {
            use connection = new NpgsqlConnection(this.ConnectionString)
            do! connection.OpenAsync()

            // 勘定科目を登録
            let insertAccountSql = @"
                INSERT INTO ""勘定科目マスタ"" (""勘定科目コード"", ""勘定科目名"", ""勘定科目種別"", ""課税取引コード"", ""残高"")
                VALUES (@code, @name, @type::account_type, @taxCode, @balance)
                RETURNING ""勘定科目ID""
            "

            use insertCmd = new NpgsqlCommand(insertAccountSql, connection)
            insertCmd.Parameters.AddWithValue("code", "4100") |> ignore
            insertCmd.Parameters.AddWithValue("name", "売上高") |> ignore
            insertCmd.Parameters.AddWithValue("type", "収益") |> ignore
            insertCmd.Parameters.AddWithValue("taxCode", "01") |> ignore
            insertCmd.Parameters.AddWithValue("balance", 0m) |> ignore
            let! accountId = insertCmd.ExecuteScalarAsync()
            let accountIdInt = Convert.ToInt32(accountId)

            // 課税取引との関連を確認（新しい接続を使用）
            use connection2 = new NpgsqlConnection(this.ConnectionString)
            do! connection2.OpenAsync()

            let selectSql = @"
                SELECT a.""勘定科目名"", t.""課税取引名"", t.""税率""
                FROM ""勘定科目マスタ"" a
                JOIN ""課税取引マスタ"" t ON a.""課税取引コード"" = t.""課税取引コード""
                WHERE a.""勘定科目ID"" = @id
            "

            use selectCmd = new NpgsqlCommand(selectSql, connection2)
            selectCmd.Parameters.AddWithValue("id", accountIdInt) |> ignore
            use! reader = selectCmd.ExecuteReaderAsync()

            let! hasData = reader.ReadAsync()
            hasData |> should equal true
            reader.GetString(reader.GetOrdinal("勘定科目名")) |> should equal "売上高"
            reader.GetString(reader.GetOrdinal("課税取引名")) |> should equal "課税売上10%"
            reader.GetDecimal(reader.GetOrdinal("税率")) |> should equal 10.00m

            // クリーンアップ（新しい接続を使用）
            use connection3 = new NpgsqlConnection(this.ConnectionString)
            do! connection3.OpenAsync()

            let deleteSql = @"DELETE FROM ""勘定科目マスタ"" WHERE ""勘定科目ID"" = @id"
            use deleteCmd = new NpgsqlCommand(deleteSql, connection3)
            deleteCmd.Parameters.AddWithValue("id", accountIdInt) |> ignore
            let! _ = deleteCmd.ExecuteNonQueryAsync()
            ()
        }
