module AccountingSystem.Infrastructure.Repositories.TaxTransactionRepository

open AccountingSystem.Domain.Models
open Dapper
open Npgsql
open System

/// 課税取引コードで検索
let findByCodeAsync (connectionString: string) (taxCode: string) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        let sql = """
            SELECT "課税取引コード" AS "TaxCode",
                   "課税取引名" AS "TaxName",
                   "税率" AS "TaxRate",
                   "説明" AS "Description"
            FROM "課税取引マスタ"
            WHERE "課税取引コード" = @TaxCode
        """

        let! results = conn.QueryAsync<TaxTransaction>(sql, {| TaxCode = taxCode |})
        return results |> Seq.tryHead
    }

/// 全ての課税取引を取得
let findAllAsync (connectionString: string) =
    task {
        use conn = new NpgsqlConnection(connectionString)
        do! conn.OpenAsync()

        let sql = """
            SELECT "課税取引コード" AS "TaxCode",
                   "課税取引名" AS "TaxName",
                   "税率" AS "TaxRate",
                   "説明" AS "Description"
            FROM "課税取引マスタ"
            ORDER BY "課税取引コード"
        """

        let! results = conn.QueryAsync<TaxTransaction>(sql)
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
                SELECT "課税取引コード" AS "TaxCode",
                       "課税取引名" AS "TaxName",
                       "税率" AS "TaxRate",
                       "説明" AS "Description"
                FROM "課税取引マスタ"
                %s
                ORDER BY "課税取引コード"
                """
                whereClause

        let! results = conn.QueryAsync<TaxTransaction>(sql, parameters)
        return results |> Seq.toList
    }
