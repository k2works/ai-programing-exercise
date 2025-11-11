module SalesManagement.Infrastructure.Repositories.CompanyCategoryRepository

open System.Threading.Tasks
open Npgsql
open Dapper
open SalesManagement.Domain.Models

let findByIdAsync (connectionString: string) (categoryTypeCode: string) (categoryCode: string) : Task<CompanyCategory option> =
    task {
        let sql = """
            SELECT 分類種別コード AS CategoryTypeCode,
                   分類コード AS CategoryCode,
                   分類名 AS CategoryName,
                   作成日時 AS CreatedAt,
                   作成者名 AS CreatedBy,
                   更新日時 AS UpdatedAt,
                   更新者名 AS UpdatedBy
            FROM 取引先分類マスタ
            WHERE 分類種別コード = @CategoryTypeCode
              AND 分類コード = @CategoryCode"""
        use connection = new NpgsqlConnection(connectionString)
        let! result = connection.QuerySingleOrDefaultAsync<CompanyCategory>(sql, {| CategoryTypeCode = categoryTypeCode; CategoryCode = categoryCode |})
        return if isNull (box result) then None else Some result
    }

let findByCategoryTypeAsync (connectionString: string) (categoryTypeCode: string) : Task<CompanyCategory seq> =
    task {
        let sql = """
            SELECT 分類種別コード AS CategoryTypeCode,
                   分類コード AS CategoryCode,
                   分類名 AS CategoryName,
                   作成日時 AS CreatedAt,
                   作成者名 AS CreatedBy,
                   更新日時 AS UpdatedAt,
                   更新者名 AS UpdatedBy
            FROM 取引先分類マスタ
            WHERE 分類種別コード = @CategoryTypeCode
            ORDER BY 分類コード"""
        use connection = new NpgsqlConnection(connectionString)
        let! result = connection.QueryAsync<CompanyCategory>(sql, {| CategoryTypeCode = categoryTypeCode |})
        return result
    }

let findAllAsync (connectionString: string) : Task<CompanyCategory seq> =
    task {
        let sql = """
            SELECT 分類種別コード AS CategoryTypeCode,
                   分類コード AS CategoryCode,
                   分類名 AS CategoryName,
                   作成日時 AS CreatedAt,
                   作成者名 AS CreatedBy,
                   更新日時 AS UpdatedAt,
                   更新者名 AS UpdatedBy
            FROM 取引先分類マスタ
            ORDER BY 分類種別コード, 分類コード"""
        use connection = new NpgsqlConnection(connectionString)
        let! result = connection.QueryAsync<CompanyCategory>(sql)
        return result
    }

let insertAsync (connectionString: string) (category: CompanyCategory) : Task<unit> =
    task {
        let sql = """
            INSERT INTO 取引先分類マスタ (
                分類種別コード,
                分類コード,
                分類名,
                作成日時,
                作成者名,
                更新日時,
                更新者名
            ) VALUES (
                @CategoryTypeCode,
                @CategoryCode,
                @CategoryName,
                @CreatedAt,
                @CreatedBy,
                @UpdatedAt,
                @UpdatedBy
            )"""
        use connection = new NpgsqlConnection(connectionString)
        let! _ = connection.ExecuteAsync(sql, category)
        return ()
    }

let updateAsync (connectionString: string) (category: CompanyCategory) : Task<unit> =
    task {
        let sql = """
            UPDATE 取引先分類マスタ
            SET 分類名 = @CategoryName,
                更新日時 = @UpdatedAt,
                更新者名 = @UpdatedBy
            WHERE 分類種別コード = @CategoryTypeCode
              AND 分類コード = @CategoryCode"""
        use connection = new NpgsqlConnection(connectionString)
        let! _ = connection.ExecuteAsync(sql, category)
        return ()
    }

let deleteAsync (connectionString: string) (categoryTypeCode: string) (categoryCode: string) : Task<unit> =
    task {
        let sql = """
            DELETE FROM 取引先分類マスタ
            WHERE 分類種別コード = @CategoryTypeCode
              AND 分類コード = @CategoryCode"""
        use connection = new NpgsqlConnection(connectionString)
        let! _ = connection.ExecuteAsync(sql, {| CategoryTypeCode = categoryTypeCode; CategoryCode = categoryCode |})
        return ()
    }
