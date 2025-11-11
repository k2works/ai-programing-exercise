module SalesManagement.Infrastructure.Repositories.PriceByCustomerRepository

open System
open System.Threading.Tasks
open Dapper
open Npgsql
open SalesManagement.Domain.Models

/// <summary>
/// 顧客別販売単価を登録
/// </summary>
let insertAsync (connectionString: string) (price: PriceByCustomer) : Task<unit> =
    task {
        let sql = """
            INSERT INTO 顧客別販売単価 (
                商品コード, 取引先コード, 販売単価,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @ProductCode, @CustomerCode, @SellingPrice,
                @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
            )"""

        use connection = new NpgsqlConnection(connectionString)
        let! _ = connection.ExecuteAsync(sql, price)
        return ()
    }

/// <summary>
/// 顧客別販売単価を検索（商品コード + 取引先コード）
/// </summary>
let findByIdAsync (connectionString: string) (productCode: string) (customerCode: string) : Task<PriceByCustomer option> =
    task {
        let sql = """
            SELECT
                商品コード AS ProductCode,
                取引先コード AS CustomerCode,
                販売単価 AS SellingPrice,
                作成日時 AS CreatedAt,
                作成者名 AS CreatedBy,
                更新日時 AS UpdatedAt,
                更新者名 AS UpdatedBy
            FROM 顧客別販売単価
            WHERE 商品コード = @ProductCode
              AND 取引先コード = @CustomerCode"""

        use connection = new NpgsqlConnection(connectionString)
        let! result = connection.QuerySingleOrDefaultAsync<PriceByCustomer>(sql, {| ProductCode = productCode; CustomerCode = customerCode |})
        return if isNull (box result) then None else Some result
    }

/// <summary>
/// 商品コードで顧客別販売単価を検索
/// </summary>
let findByProductCodeAsync (connectionString: string) (productCode: string) : Task<seq<PriceByCustomer>> =
    task {
        let sql = """
            SELECT
                商品コード AS ProductCode,
                取引先コード AS CustomerCode,
                販売単価 AS SellingPrice,
                作成日時 AS CreatedAt,
                作成者名 AS CreatedBy,
                更新日時 AS UpdatedAt,
                更新者名 AS UpdatedBy
            FROM 顧客別販売単価
            WHERE 商品コード = @ProductCode
            ORDER BY 取引先コード"""

        use connection = new NpgsqlConnection(connectionString)
        let! results = connection.QueryAsync<PriceByCustomer>(sql, {| ProductCode = productCode |})
        return results :> seq<PriceByCustomer>
    }
