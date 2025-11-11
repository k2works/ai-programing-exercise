module SalesManagement.Infrastructure.Repositories.CompanyCategoryGroupRepository

open System.Threading.Tasks
open Npgsql
open Dapper
open SalesManagement.Domain.Models

let findByIdAsync (connectionString: string) (categoryTypeCode: string) (categoryCode: string) (companyCode: string) : Task<CompanyCategoryGroup option> =
    task {
        let sql = """
            SELECT 分類種別コード AS CategoryTypeCode,
                   分類コード AS CategoryCode,
                   取引先コード AS CompanyCode,
                   作成日時 AS CreatedAt,
                   作成者名 AS CreatedBy,
                   更新日時 AS UpdatedAt,
                   更新者名 AS UpdatedBy
            FROM 取引先分類所属マスタ
            WHERE 分類種別コード = @CategoryTypeCode
              AND 分類コード = @CategoryCode
              AND 取引先コード = @CompanyCode"""
        use connection = new NpgsqlConnection(connectionString)
        let! result = connection.QuerySingleOrDefaultAsync<CompanyCategoryGroup>(sql, {| CategoryTypeCode = categoryTypeCode; CategoryCode = categoryCode; CompanyCode = companyCode |})
        return if isNull (box result) then None else Some result
    }

let findByCompanyCodeAsync (connectionString: string) (companyCode: string) : Task<CompanyCategoryGroup seq> =
    task {
        let sql = """
            SELECT 分類種別コード AS CategoryTypeCode,
                   分類コード AS CategoryCode,
                   取引先コード AS CompanyCode,
                   作成日時 AS CreatedAt,
                   作成者名 AS CreatedBy,
                   更新日時 AS UpdatedAt,
                   更新者名 AS UpdatedBy
            FROM 取引先分類所属マスタ
            WHERE 取引先コード = @CompanyCode
            ORDER BY 分類種別コード, 分類コード"""
        use connection = new NpgsqlConnection(connectionString)
        let! result = connection.QueryAsync<CompanyCategoryGroup>(sql, {| CompanyCode = companyCode |})
        return result
    }

let findByCategoryAsync (connectionString: string) (categoryTypeCode: string) (categoryCode: string) : Task<CompanyCategoryGroup seq> =
    task {
        let sql = """
            SELECT 分類種別コード AS CategoryTypeCode,
                   分類コード AS CategoryCode,
                   取引先コード AS CompanyCode,
                   作成日時 AS CreatedAt,
                   作成者名 AS CreatedBy,
                   更新日時 AS UpdatedAt,
                   更新者名 AS UpdatedBy
            FROM 取引先分類所属マスタ
            WHERE 分類種別コード = @CategoryTypeCode
              AND 分類コード = @CategoryCode
            ORDER BY 取引先コード"""
        use connection = new NpgsqlConnection(connectionString)
        let! result = connection.QueryAsync<CompanyCategoryGroup>(sql, {| CategoryTypeCode = categoryTypeCode; CategoryCode = categoryCode |})
        return result
    }

let insertAsync (connectionString: string) (categoryGroup: CompanyCategoryGroup) : Task<unit> =
    task {
        let sql = """
            INSERT INTO 取引先分類所属マスタ (
                分類種別コード,
                分類コード,
                取引先コード,
                作成日時,
                作成者名,
                更新日時,
                更新者名
            ) VALUES (
                @CategoryTypeCode,
                @CategoryCode,
                @CompanyCode,
                @CreatedAt,
                @CreatedBy,
                @UpdatedAt,
                @UpdatedBy
            )"""
        use connection = new NpgsqlConnection(connectionString)
        let! _ = connection.ExecuteAsync(sql, categoryGroup)
        return ()
    }

let deleteAsync (connectionString: string) (categoryTypeCode: string) (categoryCode: string) (companyCode: string) : Task<unit> =
    task {
        let sql = """
            DELETE FROM 取引先分類所属マスタ
            WHERE 分類種別コード = @CategoryTypeCode
              AND 分類コード = @CategoryCode
              AND 取引先コード = @CompanyCode"""
        use connection = new NpgsqlConnection(connectionString)
        let! _ = connection.ExecuteAsync(sql, {| CategoryTypeCode = categoryTypeCode; CategoryCode = categoryCode; CompanyCode = companyCode |})
        return ()
    }
