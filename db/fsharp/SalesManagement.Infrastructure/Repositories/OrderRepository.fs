module SalesManagement.Infrastructure.Repositories.OrderRepository

open System.Threading.Tasks
open Npgsql
open Dapper
open SalesManagement.Domain.Models

let findByIdAsync (connectionString: string) (orderNo: string) : Task<Order option> =
    task {
        let sql = """
            SELECT 受注番号 AS OrderNo,
                   受注日 AS OrderDate,
                   得意先コード AS CustomerCode,
                   得意先枝番 AS CustomerBranch,
                   社員コード AS EmployeeCode,
                   納期 AS DueDate,
                   受注金額合計 AS OrderAmount,
                   消費税合計 AS ConsumptionTax,
                   伝票備考 AS SlipComment,
                   部門コード AS DepartmentCode,
                   作成日時 AS CreatedAt,
                   作成者名 AS CreatedBy,
                   更新日時 AS UpdatedAt,
                   更新者名 AS UpdatedBy
            FROM 受注データ
            WHERE 受注番号 = @OrderNo"""
        use connection = new NpgsqlConnection(connectionString)
        let! result = connection.QuerySingleOrDefaultAsync<Order>(sql, {| OrderNo = orderNo |})
        return if isNull (box result) then None else Some result
    }

let findByCustomerAsync (connectionString: string) (customerCode: string) (customerBranch: int) : Task<Order seq> =
    task {
        let sql = """
            SELECT 受注番号 AS OrderNo,
                   受注日 AS OrderDate,
                   得意先コード AS CustomerCode,
                   得意先枝番 AS CustomerBranch,
                   社員コード AS EmployeeCode,
                   納期 AS DueDate,
                   受注金額合計 AS OrderAmount,
                   消費税合計 AS ConsumptionTax,
                   伝票備考 AS SlipComment,
                   部門コード AS DepartmentCode,
                   作成日時 AS CreatedAt,
                   作成者名 AS CreatedBy,
                   更新日時 AS UpdatedAt,
                   更新者名 AS UpdatedBy
            FROM 受注データ
            WHERE 得意先コード = @CustomerCode
              AND 得意先枝番 = @CustomerBranch
            ORDER BY 受注日 DESC, 受注番号 DESC"""
        use connection = new NpgsqlConnection(connectionString)
        let! results = connection.QueryAsync<Order>(sql, {| CustomerCode = customerCode; CustomerBranch = customerBranch |})
        return results
    }

let findAllAsync (connectionString: string) : Task<Order seq> =
    task {
        let sql = """
            SELECT 受注番号 AS OrderNo,
                   受注日 AS OrderDate,
                   得意先コード AS CustomerCode,
                   得意先枝番 AS CustomerBranch,
                   社員コード AS EmployeeCode,
                   納期 AS DueDate,
                   受注金額合計 AS OrderAmount,
                   消費税合計 AS ConsumptionTax,
                   伝票備考 AS SlipComment,
                   部門コード AS DepartmentCode,
                   作成日時 AS CreatedAt,
                   作成者名 AS CreatedBy,
                   更新日時 AS UpdatedAt,
                   更新者名 AS UpdatedBy
            FROM 受注データ
            ORDER BY 受注日 DESC, 受注番号 DESC"""
        use connection = new NpgsqlConnection(connectionString)
        let! results = connection.QueryAsync<Order>(sql)
        return results
    }

let insertAsync (connectionString: string) (order: Order) : Task<unit> =
    task {
        let sql = """
            INSERT INTO 受注データ (
                受注番号, 受注日, 得意先コード, 得意先枝番, 社員コード,
                納期, 受注金額合計, 消費税合計, 伝票備考, 部門コード,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @OrderNo, @OrderDate, @CustomerCode, @CustomerBranch, @EmployeeCode,
                @DueDate, @OrderAmount, @ConsumptionTax, @SlipComment, @DepartmentCode,
                @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
            )"""
        use connection = new NpgsqlConnection(connectionString)
        let! _ = connection.ExecuteAsync(sql, order)
        return ()
    }

let updateAsync (connectionString: string) (order: Order) : Task<unit> =
    task {
        let sql = """
            UPDATE 受注データ SET
                受注日 = @OrderDate,
                得意先コード = @CustomerCode,
                得意先枝番 = @CustomerBranch,
                社員コード = @EmployeeCode,
                納期 = @DueDate,
                受注金額合計 = @OrderAmount,
                消費税合計 = @ConsumptionTax,
                伝票備考 = @SlipComment,
                部門コード = @DepartmentCode,
                更新日時 = @UpdatedAt,
                更新者名 = @UpdatedBy
            WHERE 受注番号 = @OrderNo"""
        use connection = new NpgsqlConnection(connectionString)
        let! _ = connection.ExecuteAsync(sql, order)
        return ()
    }

let deleteAsync (connectionString: string) (orderNo: string) : Task<unit> =
    task {
        let sql = "DELETE FROM 受注データ WHERE 受注番号 = @OrderNo"
        use connection = new NpgsqlConnection(connectionString)
        let! _ = connection.ExecuteAsync(sql, {| OrderNo = orderNo |})
        return ()
    }
