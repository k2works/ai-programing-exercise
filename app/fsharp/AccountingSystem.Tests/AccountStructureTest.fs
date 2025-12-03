module AccountingSystem.Tests.AccountStructureTest

open Xunit
open FsUnit.Xunit
open Npgsql
open System
open AccountingSystem.Tests.DatabaseTestBase

/// <summary>
/// 勘定科目構成マスタのテスト
/// </summary>
type AccountStructureTest() =
    inherit DatabaseTestBase()

    /// <summary>
    /// テスト用の勘定科目と構成を登録
    /// </summary>
    member private this.SetupTestDataAsync() =
        task {
            use connection = new NpgsqlConnection(this.ConnectionString)
            do! connection.OpenAsync()

            // 勘定科目を登録
            let insertAccountSql = @"
                INSERT INTO ""勘定科目マスタ"" (""勘定科目コード"", ""勘定科目名"", ""勘定科目種別"", ""合計科目"", ""残高"")
                VALUES (@code, @name, @type::account_type, @isSummary, @balance)
            "

            let accounts = [
                ("11", "資産の部", "資産", true, 0m)
                ("11000", "流動資産", "資産", true, 0m)
                ("11190", "現金及び預金", "資産", true, 0m)
                ("11110", "現金", "資産", false, 50000m)
                ("11120", "当座預金", "資産", false, 100000m)
                ("11130", "普通預金", "資産", false, 200000m)
            ]

            for (code, name, accountType, isSummary, balance) in accounts do
                use cmd = new NpgsqlCommand(insertAccountSql, connection)
                cmd.Parameters.AddWithValue("code", code) |> ignore
                cmd.Parameters.AddWithValue("name", name) |> ignore
                cmd.Parameters.AddWithValue("type", accountType) |> ignore
                cmd.Parameters.AddWithValue("isSummary", isSummary) |> ignore
                cmd.Parameters.AddWithValue("balance", balance) |> ignore
                let! _ = cmd.ExecuteNonQueryAsync()
                ()

            // 勘定科目構成を登録
            let insertStructureSql = @"
                INSERT INTO ""勘定科目構成マスタ"" (""勘定科目コード"", ""勘定科目パス"", ""階層レベル"", ""親科目コード"", ""表示順序"")
                VALUES (@code, @path, @level, @parent, @order)
            "

            let structures = [
                ("11", "11", 1, None, 1)
                ("11000", "11~11000", 2, Some "11", 1)
                ("11190", "11~11000~11190", 3, Some "11000", 1)
                ("11110", "11~11000~11190~11110", 4, Some "11190", 1)
                ("11120", "11~11000~11190~11120", 4, Some "11190", 2)
                ("11130", "11~11000~11190~11130", 4, Some "11190", 3)
            ]

            for (code, path, level, parent, order) in structures do
                use cmd = new NpgsqlCommand(insertStructureSql, connection)
                cmd.Parameters.AddWithValue("code", code) |> ignore
                cmd.Parameters.AddWithValue("path", path) |> ignore
                cmd.Parameters.AddWithValue("level", level) |> ignore
                match parent with
                | Some p -> cmd.Parameters.AddWithValue("parent", p) |> ignore
                | None -> cmd.Parameters.AddWithValue("parent", DBNull.Value) |> ignore
                cmd.Parameters.AddWithValue("order", order) |> ignore
                let! _ = cmd.ExecuteNonQueryAsync()
                ()
        }

    /// <summary>
    /// テストデータをクリーンアップ
    /// </summary>
    member private this.CleanupTestDataAsync() =
        task {
            use connection = new NpgsqlConnection(this.ConnectionString)
            do! connection.OpenAsync()

            use cmd = new NpgsqlCommand(@"
                DELETE FROM ""勘定科目構成マスタ"";
                DELETE FROM ""勘定科目マスタ"";
            ", connection)
            let! _ = cmd.ExecuteNonQueryAsync()
            ()
        }

    [<Fact>]
    member this.``勘定科目構成を登録できる``() =
        task {
            do! this.CleanupTestDataAsync()
            do! this.SetupTestDataAsync()

            use connection = new NpgsqlConnection(this.ConnectionString)
            do! connection.OpenAsync()

            let sql = @"
                SELECT COUNT(*) FROM ""勘定科目構成マスタ""
            "

            use cmd = new NpgsqlCommand(sql, connection)
            let! count = cmd.ExecuteScalarAsync()

            Convert.ToInt64(count) |> should equal 6L

            do! this.CleanupTestDataAsync()
        }

    [<Fact>]
    member this.``チルダ連結パスで子孫科目を検索できる``() =
        task {
            do! this.CleanupTestDataAsync()
            do! this.SetupTestDataAsync()

            use connection = new NpgsqlConnection(this.ConnectionString)
            do! connection.OpenAsync()

            // 「現金及び預金」(11190) 配下の科目を検索
            let sql = @"
                SELECT s.""勘定科目コード"", a.""勘定科目名""
                FROM ""勘定科目構成マスタ"" s
                JOIN ""勘定科目マスタ"" a ON s.""勘定科目コード"" = a.""勘定科目コード""
                WHERE s.""勘定科目パス"" LIKE '%~11190~%'
                   OR s.""勘定科目コード"" = '11190'
                ORDER BY s.""勘定科目パス""
            "

            let codes = ResizeArray<string>()
            use cmd = new NpgsqlCommand(sql, connection)
            use! reader = cmd.ExecuteReaderAsync()

            while reader.Read() do
                codes.Add(reader.GetString(reader.GetOrdinal("勘定科目コード")))

            codes.Count |> should equal 4
            codes |> should contain "11190"
            codes |> should contain "11110"
            codes |> should contain "11120"
            codes |> should contain "11130"

            do! this.CleanupTestDataAsync()
        }

    [<Fact>]
    member this.``特定科目配下の残高を集計できる``() =
        task {
            do! this.CleanupTestDataAsync()
            do! this.SetupTestDataAsync()

            use connection = new NpgsqlConnection(this.ConnectionString)
            do! connection.OpenAsync()

            // 「現金及び預金」(11190) 配下の残高を集計
            let sql = @"
                SELECT SUM(a.""残高"") as 合計残高
                FROM ""勘定科目マスタ"" a
                JOIN ""勘定科目構成マスタ"" s ON a.""勘定科目コード"" = s.""勘定科目コード""
                WHERE (s.""勘定科目パス"" LIKE '%~11190~%' OR s.""勘定科目コード"" = '11190')
                  AND a.""合計科目"" = false
            "

            use cmd = new NpgsqlCommand(sql, connection)
            let! result = cmd.ExecuteScalarAsync()

            Convert.ToDecimal(result) |> should equal 350000m  // 50000 + 100000 + 200000

            do! this.CleanupTestDataAsync()
        }

    [<Fact>]
    member this.``階層レベルで科目をフィルタリングできる``() =
        task {
            do! this.CleanupTestDataAsync()
            do! this.SetupTestDataAsync()

            use connection = new NpgsqlConnection(this.ConnectionString)
            do! connection.OpenAsync()

            // 階層レベル4（明細科目）を取得
            let sql = @"
                SELECT s.""勘定科目コード"", a.""勘定科目名""
                FROM ""勘定科目構成マスタ"" s
                JOIN ""勘定科目マスタ"" a ON s.""勘定科目コード"" = a.""勘定科目コード""
                WHERE s.""階層レベル"" = 4
                ORDER BY s.""表示順序""
            "

            let names = ResizeArray<string>()
            use cmd = new NpgsqlCommand(sql, connection)
            use! reader = cmd.ExecuteReaderAsync()

            while reader.Read() do
                names.Add(reader.GetString(reader.GetOrdinal("勘定科目名")))

            names.Count |> should equal 3
            names.[0] |> should equal "現金"
            names.[1] |> should equal "当座預金"
            names.[2] |> should equal "普通預金"

            do! this.CleanupTestDataAsync()
        }
