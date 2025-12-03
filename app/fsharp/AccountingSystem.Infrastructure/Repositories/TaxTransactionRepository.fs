module AccountingSystem.Infrastructure.Repositories.TaxTransactionRepository

open Dapper
open Npgsql
open System
open System.Threading.Tasks

/// <summary>
/// 課税取引エンティティレコード
/// </summary>
type TaxTransactionEntity = {
    TaxCode: string
    TaxName: string
    TaxRate: decimal
    Description: string
    CreatedAt: DateTime
    UpdatedAt: DateTime
}

/// 課税取引コードで検索
let findByCodeAsync (connectionString: string) (taxCode: string) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        let sql = """
            SELECT "課税取引コード", "課税取引名", "税率", "説明", "作成日時", "更新日時"
            FROM "課税取引マスタ"
            WHERE "課税取引コード" = @TaxCode
        """

        use cmd = new NpgsqlCommand(sql, conn)
        cmd.Parameters.AddWithValue("TaxCode", taxCode) |> ignore
        use! reader = cmd.ExecuteReaderAsync()

        let! hasData = reader.ReadAsync()
        if hasData then
            let description =
                if reader.IsDBNull(reader.GetOrdinal("説明")) then ""
                else reader.GetString(reader.GetOrdinal("説明"))

            return Some {
                TaxCode = reader.GetString(reader.GetOrdinal("課税取引コード"))
                TaxName = reader.GetString(reader.GetOrdinal("課税取引名"))
                TaxRate = reader.GetDecimal(reader.GetOrdinal("税率"))
                Description = description
                CreatedAt = reader.GetDateTime(reader.GetOrdinal("作成日時"))
                UpdatedAt = reader.GetDateTime(reader.GetOrdinal("更新日時"))
            }
        else
            return None
    }

/// 全ての課税取引を取得
let findAllAsync (connectionString: string) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        let sql = """
            SELECT "課税取引コード", "課税取引名", "税率", "説明", "作成日時", "更新日時"
            FROM "課税取引マスタ"
            ORDER BY "課税取引コード"
        """

        use cmd = new NpgsqlCommand(sql, conn)
        use! reader = cmd.ExecuteReaderAsync()

        let results = ResizeArray<TaxTransactionEntity>()
        while reader.Read() do
            let description =
                if reader.IsDBNull(reader.GetOrdinal("説明")) then ""
                else reader.GetString(reader.GetOrdinal("説明"))

            results.Add({
                TaxCode = reader.GetString(reader.GetOrdinal("課税取引コード"))
                TaxName = reader.GetString(reader.GetOrdinal("課税取引名"))
                TaxRate = reader.GetDecimal(reader.GetOrdinal("税率"))
                Description = description
                CreatedAt = reader.GetDateTime(reader.GetOrdinal("作成日時"))
                UpdatedAt = reader.GetDateTime(reader.GetOrdinal("更新日時"))
            })

        return results |> Seq.toList
    }

/// 動的 SQL による条件検索
let searchAsync
    (connectionString: string)
    (taxName: string option)
    (minTaxRate: decimal option) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        // 条件とパラメータを構築
        let conditions = ResizeArray<string>()
        let parameters = DynamicParameters()

        // 課税取引名での部分一致検索
        match taxName with
        | Some name when not (String.IsNullOrEmpty name) ->
            conditions.Add(""""課税取引名" LIKE CONCAT('%', @TaxName, '%')""")
            parameters.Add("TaxName", name)
        | _ -> ()

        // 最小税率での検索
        match minTaxRate with
        | Some rate ->
            conditions.Add(""""税率" >= @MinTaxRate""")
            parameters.Add("MinTaxRate", rate)
        | None -> ()

        // WHERE 句を構築
        let whereClause =
            if conditions.Count > 0 then
                "WHERE " + String.Join(" AND ", conditions)
            else
                ""

        let sql =
            sprintf
                """
                SELECT "課税取引コード", "課税取引名", "税率", "説明", "作成日時", "更新日時"
                FROM "課税取引マスタ"
                %s
                ORDER BY "課税取引コード"
                """
                whereClause

        use cmd = new NpgsqlCommand(sql, conn)

        match taxName with
        | Some name when not (String.IsNullOrEmpty name) ->
            cmd.Parameters.AddWithValue("TaxName", name) |> ignore
        | _ -> ()

        match minTaxRate with
        | Some rate ->
            cmd.Parameters.AddWithValue("MinTaxRate", rate) |> ignore
        | None -> ()

        use! reader = cmd.ExecuteReaderAsync()

        let results = ResizeArray<TaxTransactionEntity>()
        while reader.Read() do
            let description =
                if reader.IsDBNull(reader.GetOrdinal("説明")) then ""
                else reader.GetString(reader.GetOrdinal("説明"))

            results.Add({
                TaxCode = reader.GetString(reader.GetOrdinal("課税取引コード"))
                TaxName = reader.GetString(reader.GetOrdinal("課税取引名"))
                TaxRate = reader.GetDecimal(reader.GetOrdinal("税率"))
                Description = description
                CreatedAt = reader.GetDateTime(reader.GetOrdinal("作成日時"))
                UpdatedAt = reader.GetDateTime(reader.GetOrdinal("更新日時"))
            })

        return results |> Seq.toList
    }
