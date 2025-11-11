module SalesManagement.Infrastructure.Repositories.SalesRepository

open System.Threading.Tasks
open Npgsql
open Dapper
open SalesManagement.Domain.Models

let findByIdAsync (connectionString: string) (salesSlipNo: string) : Task<Sales option> =
    task {
        let sql = """
            SELECT 売上伝票番号 AS SalesSlipNo,
                   売上日 AS SalesDate,
                   受注番号 AS OrderNo,
                   得意先コード AS CustomerCode,
                   得意先枝番 AS CustomerBranch,
                   社員コード AS EmployeeCode,
                   売上金額合計 AS SalesAmount,
                   消費税合計 AS ConsumptionTax,
                   伝票備考 AS SlipComment,
                   部門コード AS DepartmentCode,
                   作成日時 AS CreatedAt,
                   作成者名 AS CreatedBy,
                   更新日時 AS UpdatedAt,
                   更新者名 AS UpdatedBy
            FROM 売上データ
            WHERE 売上伝票番号 = @SalesSlipNo"""
        use connection = new NpgsqlConnection(connectionString)
        let! result = connection.QuerySingleOrDefaultAsync<Sales>(sql, {| SalesSlipNo = salesSlipNo |})
        return if isNull (box result) then None else Some result
    }

let findByOrderNoAsync (connectionString: string) (orderNo: string) : Task<Sales seq> =
    task {
        let sql = """
            SELECT 売上伝票番号 AS SalesSlipNo,
                   売上日 AS SalesDate,
                   受注番号 AS OrderNo,
                   得意先コード AS CustomerCode,
                   得意先枝番 AS CustomerBranch,
                   社員コード AS EmployeeCode,
                   売上金額合計 AS SalesAmount,
                   消費税合計 AS ConsumptionTax,
                   伝票備考 AS SlipComment,
                   部門コード AS DepartmentCode,
                   作成日時 AS CreatedAt,
                   作成者名 AS CreatedBy,
                   更新日時 AS UpdatedAt,
                   更新者名 AS UpdatedBy
            FROM 売上データ
            WHERE 受注番号 = @OrderNo
            ORDER BY 売上日 DESC"""
        use connection = new NpgsqlConnection(connectionString)
        let! results = connection.QueryAsync<Sales>(sql, {| OrderNo = orderNo |})
        return results
    }

let findByCustomerAsync (connectionString: string) (customerCode: string) (customerBranch: int) : Task<Sales seq> =
    task {
        let sql = """
            SELECT 売上伝票番号 AS SalesSlipNo,
                   売上日 AS SalesDate,
                   受注番号 AS OrderNo,
                   得意先コード AS CustomerCode,
                   得意先枝番 AS CustomerBranch,
                   社員コード AS EmployeeCode,
                   売上金額合計 AS SalesAmount,
                   消費税合計 AS ConsumptionTax,
                   伝票備考 AS SlipComment,
                   部門コード AS DepartmentCode,
                   作成日時 AS CreatedAt,
                   作成者名 AS CreatedBy,
                   更新日時 AS UpdatedAt,
                   更新者名 AS UpdatedBy
            FROM 売上データ
            WHERE 得意先コード = @CustomerCode
              AND 得意先枝番 = @CustomerBranch
            ORDER BY 売上日 DESC"""
        use connection = new NpgsqlConnection(connectionString)
        let! results = connection.QueryAsync<Sales>(sql, {| CustomerCode = customerCode; CustomerBranch = customerBranch |})
        return results
    }

let insertAsync (connectionString: string) (sales: Sales) : Task<unit> =
    task {
        let sql = """
            INSERT INTO 売上データ (
                売上伝票番号, 売上日, 受注番号, 得意先コード, 得意先枝番, 社員コード,
                売上金額合計, 消費税合計, 伝票備考, 部門コード,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @SalesSlipNo, @SalesDate, @OrderNo, @CustomerCode, @CustomerBranch, @EmployeeCode,
                @SalesAmount, @ConsumptionTax, @SlipComment, @DepartmentCode,
                @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
            )"""
        use connection = new NpgsqlConnection(connectionString)
        let! _ = connection.ExecuteAsync(sql, sales)
        return ()
    }

let deleteAsync (connectionString: string) (salesSlipNo: string) : Task<unit> =
    task {
        let sql = "DELETE FROM 売上データ WHERE 売上伝票番号 = @SalesSlipNo"
        use connection = new NpgsqlConnection(connectionString)
        let! _ = connection.ExecuteAsync(sql, {| SalesSlipNo = salesSlipNo |})
        return ()
    }
