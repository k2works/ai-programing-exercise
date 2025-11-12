module SalesManagement.Infrastructure.Repositories.PurchaseRepository

open System.Threading.Tasks
open Npgsql
open Dapper
open SalesManagement.Domain.Models

let findByIdAsync (connectionString: string) (purchaseNo: string) : Task<Purchase option> =
    task {
        use connection = new NpgsqlConnection(connectionString)
        let sql = """
            SELECT
                仕入番号 AS PurchaseNo,
                仕入日 AS PurchaseDate,
                発注番号 AS PoNo,
                仕入先コード AS SupplierCode,
                仕入先枝番 AS SupplierBranch,
                社員コード AS EmployeeCode,
                仕入金額合計 AS PurchaseAmount,
                消費税合計 AS ConsumptionTax,
                備考 AS SlipComment,
                部門コード AS DepartmentCode,
                作成日時 AS CreatedAt,
                作成者名 AS CreatedBy,
                更新日時 AS UpdatedAt,
                更新者名 AS UpdatedBy
            FROM 仕入データ
            WHERE 仕入番号 = @PurchaseNo
        """
        let! result = connection.QuerySingleOrDefaultAsync<Purchase>(sql, {| PurchaseNo = purchaseNo |})
        return if isNull (box result) then None else Some result
    }

let findByPoNoAsync (connectionString: string) (poNo: string) : Task<Purchase seq> =
    task {
        use connection = new NpgsqlConnection(connectionString)
        let sql = """
            SELECT
                仕入番号 AS PurchaseNo,
                仕入日 AS PurchaseDate,
                発注番号 AS PoNo,
                仕入先コード AS SupplierCode,
                仕入先枝番 AS SupplierBranch,
                社員コード AS EmployeeCode,
                仕入金額合計 AS PurchaseAmount,
                消費税合計 AS ConsumptionTax,
                備考 AS SlipComment,
                部門コード AS DepartmentCode,
                作成日時 AS CreatedAt,
                作成者名 AS CreatedBy,
                更新日時 AS UpdatedAt,
                更新者名 AS UpdatedBy
            FROM 仕入データ
            WHERE 発注番号 = @PoNo
            ORDER BY 仕入日 DESC
        """
        let! results = connection.QueryAsync<Purchase>(sql, {| PoNo = poNo |})
        return results
    }

let insertAsync (connectionString: string) (purchase: Purchase) : Task<unit> =
    task {
        use connection = new NpgsqlConnection(connectionString)
        let sql = """
            INSERT INTO 仕入データ (
                仕入番号, 仕入日, 発注番号, 仕入先コード, 仕入先枝番, 社員コード,
                仕入金額合計, 消費税合計, 備考, 部門コード,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @PurchaseNo, @PurchaseDate, @PoNo, @SupplierCode, @SupplierBranch, @EmployeeCode,
                @PurchaseAmount, @ConsumptionTax, @SlipComment, @DepartmentCode,
                @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
            )
        """
        let! _ = connection.ExecuteAsync(sql, purchase)
        return ()
    }

let deleteAsync (connectionString: string) (purchaseNo: string) : Task<unit> =
    task {
        use connection = new NpgsqlConnection(connectionString)
        let sql = "DELETE FROM 仕入データ WHERE 仕入番号 = @PurchaseNo"
        let! _ = connection.ExecuteAsync(sql, {| PurchaseNo = purchaseNo |})
        return ()
    }
