module SalesManagement.Infrastructure.Repositories.SupplierRepository

open System.Threading.Tasks
open Npgsql
open Dapper
open SalesManagement.Domain.Models

let insertAsync (connectionString: string) (supplier: Supplier) : Task<unit> =
    task {
        let sql = """
            INSERT INTO 仕入先マスタ (
                仕入先コード, 仕入先枝番, 仕入先区分,
                仕入先名, 仕入先名カナ, 自社担当者コード,
                仕入先締日, 仕入先支払月, 仕入先支払日, 仕入先支払方法,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @SupplierCode, @SupplierBranch, @SupplierType,
                @SupplierName, @SupplierNameKana, @EmployeeCode,
                @SupplierCloseDate, @SupplierPayMonths, @SupplierPayDates, @SupplierPayMethod,
                @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
            )"""
        use connection = new NpgsqlConnection(connectionString)
        let! _ = connection.ExecuteAsync(sql, supplier)
        return ()
    }

let updateAsync (connectionString: string) (supplier: Supplier) : Task<unit> =
    task {
        let sql = """
            UPDATE 仕入先マスタ
            SET 仕入先区分 = @SupplierType,
                仕入先名 = @SupplierName,
                仕入先名カナ = @SupplierNameKana,
                自社担当者コード = @EmployeeCode,
                仕入先締日 = @SupplierCloseDate,
                仕入先支払月 = @SupplierPayMonths,
                仕入先支払日 = @SupplierPayDates,
                仕入先支払方法 = @SupplierPayMethod,
                更新日時 = @UpdatedAt,
                更新者名 = @UpdatedBy
            WHERE 仕入先コード = @SupplierCode AND 仕入先枝番 = @SupplierBranch"""
        use connection = new NpgsqlConnection(connectionString)
        let! _ = connection.ExecuteAsync(sql, supplier)
        return ()
    }

let deleteAsync (connectionString: string) (supplierCode: string) (supplierBranch: int) : Task<unit> =
    task {
        let sql = "DELETE FROM 仕入先マスタ WHERE 仕入先コード = @SupplierCode AND 仕入先枝番 = @SupplierBranch"
        use connection = new NpgsqlConnection(connectionString)
        let! _ = connection.ExecuteAsync(sql, {| SupplierCode = supplierCode; SupplierBranch = supplierBranch |})
        return ()
    }

let findByIdAsync (connectionString: string) (supplierCode: string) (supplierBranch: int) : Task<Supplier option> =
    task {
        let sql = """
            SELECT 仕入先コード AS SupplierCode,
                   仕入先枝番 AS SupplierBranch,
                   仕入先区分 AS SupplierType,
                   仕入先名 AS SupplierName,
                   仕入先名カナ AS SupplierNameKana,
                   自社担当者コード AS EmployeeCode,
                   仕入先締日 AS SupplierCloseDate,
                   仕入先支払月 AS SupplierPayMonths,
                   仕入先支払日 AS SupplierPayDates,
                   仕入先支払方法 AS SupplierPayMethod,
                   作成日時 AS CreatedAt,
                   作成者名 AS CreatedBy,
                   更新日時 AS UpdatedAt,
                   更新者名 AS UpdatedBy
            FROM 仕入先マスタ
            WHERE 仕入先コード = @SupplierCode AND 仕入先枝番 = @SupplierBranch"""
        use connection = new NpgsqlConnection(connectionString)
        let! result = connection.QuerySingleOrDefaultAsync<Supplier>(sql, {| SupplierCode = supplierCode; SupplierBranch = supplierBranch |})
        return if isNull (box result) then None else Some result
    }

let findBySupplierCodeAsync (connectionString: string) (supplierCode: string) : Task<seq<Supplier>> =
    task {
        let sql = """
            SELECT 仕入先コード AS SupplierCode,
                   仕入先枝番 AS SupplierBranch,
                   仕入先区分 AS SupplierType,
                   仕入先名 AS SupplierName,
                   仕入先名カナ AS SupplierNameKana,
                   自社担当者コード AS EmployeeCode,
                   仕入先締日 AS SupplierCloseDate,
                   仕入先支払月 AS SupplierPayMonths,
                   仕入先支払日 AS SupplierPayDates,
                   仕入先支払方法 AS SupplierPayMethod,
                   作成日時 AS CreatedAt,
                   作成者名 AS CreatedBy,
                   更新日時 AS UpdatedAt,
                   更新者名 AS UpdatedBy
            FROM 仕入先マスタ
            WHERE 仕入先コード = @SupplierCode
            ORDER BY 仕入先枝番"""
        use connection = new NpgsqlConnection(connectionString)
        let! results = connection.QueryAsync<Supplier>(sql, {| SupplierCode = supplierCode |})
        return results :> seq<Supplier>
    }

let findByCompanyCodeAsync (connectionString: string) (companyCode: string) : Task<seq<Supplier>> =
    task {
        let sql = """
            SELECT 仕入先コード AS SupplierCode,
                   仕入先枝番 AS SupplierBranch,
                   仕入先区分 AS SupplierType,
                   仕入先名 AS SupplierName,
                   仕入先名カナ AS SupplierNameKana,
                   自社担当者コード AS EmployeeCode,
                   仕入先締日 AS SupplierCloseDate,
                   仕入先支払月 AS SupplierPayMonths,
                   仕入先支払日 AS SupplierPayDates,
                   仕入先支払方法 AS SupplierPayMethod,
                   作成日時 AS CreatedAt,
                   作成者名 AS CreatedBy,
                   更新日時 AS UpdatedAt,
                   更新者名 AS UpdatedBy
            FROM 仕入先マスタ
            WHERE 仕入先コード = @CompanyCode
            ORDER BY 仕入先枝番"""
        use connection = new NpgsqlConnection(connectionString)
        let! results = connection.QueryAsync<Supplier>(sql, {| CompanyCode = companyCode |})
        return results :> seq<Supplier>
    }
