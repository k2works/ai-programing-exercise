module SalesManagement.Infrastructure.Repositories.CategoryTypeRepository

open System.Threading.Tasks
open Npgsql
open Dapper
open SalesManagement.Domain.Models

let findByIdAsync (connectionString: string) (categoryTypeCode: string) : Task<CategoryType option> =
    task {
        let sql = """
            SELECT 分類種別コード AS CategoryTypeCode,
                   分類種別名 AS CategoryTypeName,
                   作成日時 AS CreatedAt,
                   作成者名 AS CreatedBy,
                   更新日時 AS UpdatedAt,
                   更新者名 AS UpdatedBy
            FROM 分類種別マスタ
            WHERE 分類種別コード = @CategoryTypeCode"""
        use connection = new NpgsqlConnection(connectionString)
        let! result = connection.QuerySingleOrDefaultAsync<CategoryType>(sql, {| CategoryTypeCode = categoryTypeCode |})
        return if isNull (box result) then None else Some result
    }

let findAllAsync (connectionString: string) : Task<CategoryType seq> =
    task {
        let sql = """
            SELECT 分類種別コード AS CategoryTypeCode,
                   分類種別名 AS CategoryTypeName,
                   作成日時 AS CreatedAt,
                   作成者名 AS CreatedBy,
                   更新日時 AS UpdatedAt,
                   更新者名 AS UpdatedBy
            FROM 分類種別マスタ
            ORDER BY 分類種別コード"""
        use connection = new NpgsqlConnection(connectionString)
        let! result = connection.QueryAsync<CategoryType>(sql)
        return result
    }

let insertAsync (connectionString: string) (categoryType: CategoryType) : Task<unit> =
    task {
        let sql = """
            INSERT INTO 分類種別マスタ (
                分類種別コード,
                分類種別名,
                作成日時,
                作成者名,
                更新日時,
                更新者名
            ) VALUES (
                @CategoryTypeCode,
                @CategoryTypeName,
                @CreatedAt,
                @CreatedBy,
                @UpdatedAt,
                @UpdatedBy
            )"""
        use connection = new NpgsqlConnection(connectionString)
        let! _ = connection.ExecuteAsync(sql, categoryType)
        return ()
    }

let updateAsync (connectionString: string) (categoryType: CategoryType) : Task<unit> =
    task {
        let sql = """
            UPDATE 分類種別マスタ
            SET 分類種別名 = @CategoryTypeName,
                更新日時 = @UpdatedAt,
                更新者名 = @UpdatedBy
            WHERE 分類種別コード = @CategoryTypeCode"""
        use connection = new NpgsqlConnection(connectionString)
        let! _ = connection.ExecuteAsync(sql, categoryType)
        return ()
    }

let deleteAsync (connectionString: string) (categoryTypeCode: string) : Task<unit> =
    task {
        let sql = """
            DELETE FROM 分類種別マスタ
            WHERE 分類種別コード = @CategoryTypeCode"""
        use connection = new NpgsqlConnection(connectionString)
        let! _ = connection.ExecuteAsync(sql, {| CategoryTypeCode = categoryTypeCode |})
        return ()
    }
