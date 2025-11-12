module SalesManagement.Infrastructure.Repositories.PurchaseOrderRepository

open System.Threading.Tasks
open Npgsql
open Dapper
open SalesManagement.Domain.Models

let findByIdAsync (connectionString: string) (poNo: string) : Task<PurchaseOrder option> =
    task {
        use connection = new NpgsqlConnection(connectionString)
        let sql = """
            SELECT
                発注番号 AS PoNo,
                発注日 AS PoDate,
                受注番号 AS OrderNo,
                仕入先コード AS SupplierCode,
                仕入先枝番 AS SupplierBranch,
                社員コード AS EmployeeCode,
                指定納期 AS DueDate,
                倉庫コード AS WarehouseCode,
                発注金額合計 AS PoAmount,
                消費税合計 AS ConsumptionTax,
                備考 AS SlipComment,
                作成日時 AS CreatedAt,
                作成者名 AS CreatedBy,
                更新日時 AS UpdatedAt,
                更新者名 AS UpdatedBy
            FROM 発注データ
            WHERE 発注番号 = @PoNo
        """
        let! result = connection.QuerySingleOrDefaultAsync<PurchaseOrder>(sql, {| PoNo = poNo |})
        return if isNull (box result) then None else Some result
    }

let findByOrderNoAsync (connectionString: string) (orderNo: string) : Task<PurchaseOrder seq> =
    task {
        use connection = new NpgsqlConnection(connectionString)
        let sql = """
            SELECT
                発注番号 AS PoNo,
                発注日 AS PoDate,
                受注番号 AS OrderNo,
                仕入先コード AS SupplierCode,
                仕入先枝番 AS SupplierBranch,
                社員コード AS EmployeeCode,
                指定納期 AS DueDate,
                倉庫コード AS WarehouseCode,
                発注金額合計 AS PoAmount,
                消費税合計 AS ConsumptionTax,
                備考 AS SlipComment,
                作成日時 AS CreatedAt,
                作成者名 AS CreatedBy,
                更新日時 AS UpdatedAt,
                更新者名 AS UpdatedBy
            FROM 発注データ
            WHERE 受注番号 = @OrderNo
            ORDER BY 発注日 DESC
        """
        let! results = connection.QueryAsync<PurchaseOrder>(sql, {| OrderNo = orderNo |})
        return results
    }

let insertAsync (connectionString: string) (po: PurchaseOrder) : Task<unit> =
    task {
        use connection = new NpgsqlConnection(connectionString)
        let sql = """
            INSERT INTO 発注データ (
                発注番号, 発注日, 受注番号, 仕入先コード, 仕入先枝番, 社員コード,
                指定納期, 倉庫コード, 発注金額合計, 消費税合計, 備考,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @PoNo, @PoDate, @OrderNo, @SupplierCode, @SupplierBranch, @EmployeeCode,
                @DueDate, @WarehouseCode, @PoAmount, @ConsumptionTax, @SlipComment,
                @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
            )
        """
        let! _ = connection.ExecuteAsync(sql, po)
        return ()
    }

let deleteAsync (connectionString: string) (poNo: string) : Task<unit> =
    task {
        use connection = new NpgsqlConnection(connectionString)
        let sql = "DELETE FROM 発注データ WHERE 発注番号 = @PoNo"
        let! _ = connection.ExecuteAsync(sql, {| PoNo = poNo |})
        return ()
    }
