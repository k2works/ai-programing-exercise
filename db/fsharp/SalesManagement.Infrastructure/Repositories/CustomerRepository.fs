module SalesManagement.Infrastructure.Repositories.CustomerRepository

open System.Threading.Tasks
open Npgsql
open Dapper
open SalesManagement.Domain.Models

let insertAsync (connectionString: string) (customer: Customer) : Task<unit> =
    task {
        let sql = """
            INSERT INTO 顧客マスタ (
                顧客コード, 顧客枝番, 顧客区分,
                請求先コード, 請求先枝番, 回収先コード, 回収先枝番,
                顧客名, 顧客名カナ, 自社担当者コード,
                顧客担当者名, 顧客部門名,
                顧客郵便番号, 顧客都道府県, 顧客住所１, 顧客住所２,
                顧客電話番号, 顧客ＦＡＸ番号, 顧客メールアドレス,
                顧客請求区分, 顧客締日１, 顧客支払月１, 顧客支払日１, 顧客支払方法１,
                顧客締日２, 顧客支払月２, 顧客支払日２, 顧客支払方法２,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @CustomerCode, @CustomerBranch, @CustomerType,
                @ArCode, @ArBranch, @PayerCode, @PayerBranch,
                @CustomerName, @CustomerNameKana, @EmployeeCode,
                @CustomerUserName, @CustomerDepartmentName,
                @CustomerZipCode, @CustomerState, @CustomerAddress1, @CustomerAddress2,
                @CustomerTel, @CustomerFax, @CustomerEmail,
                @CustomerArType, @CustomerCloseDate1, @CustomerPayMonths1, @CustomerPayDates1, @CustomerPayMethod1,
                @CustomerCloseDate2, @CustomerPayMonths2, @CustomerPayDates2, @CustomerPayMethod2,
                @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
            )"""
        use connection = new NpgsqlConnection(connectionString)
        let! _ = connection.ExecuteAsync(sql, customer)
        return ()
    }

let updateAsync (connectionString: string) (customer: Customer) : Task<unit> =
    task {
        let sql = """
            UPDATE 顧客マスタ
            SET 顧客区分 = @CustomerType,
                請求先コード = @ArCode,
                請求先枝番 = @ArBranch,
                回収先コード = @PayerCode,
                回収先枝番 = @PayerBranch,
                顧客名 = @CustomerName,
                顧客名カナ = @CustomerNameKana,
                自社担当者コード = @EmployeeCode,
                顧客担当者名 = @CustomerUserName,
                顧客部門名 = @CustomerDepartmentName,
                顧客郵便番号 = @CustomerZipCode,
                顧客都道府県 = @CustomerState,
                顧客住所１ = @CustomerAddress1,
                顧客住所２ = @CustomerAddress2,
                顧客電話番号 = @CustomerTel,
                顧客ＦＡＸ番号 = @CustomerFax,
                顧客メールアドレス = @CustomerEmail,
                顧客請求区分 = @CustomerArType,
                顧客締日１ = @CustomerCloseDate1,
                顧客支払月１ = @CustomerPayMonths1,
                顧客支払日１ = @CustomerPayDates1,
                顧客支払方法１ = @CustomerPayMethod1,
                顧客締日２ = @CustomerCloseDate2,
                顧客支払月２ = @CustomerPayMonths2,
                顧客支払日２ = @CustomerPayDates2,
                顧客支払方法２ = @CustomerPayMethod2,
                更新日時 = @UpdatedAt,
                更新者名 = @UpdatedBy
            WHERE 顧客コード = @CustomerCode AND 顧客枝番 = @CustomerBranch"""
        use connection = new NpgsqlConnection(connectionString)
        let! _ = connection.ExecuteAsync(sql, customer)
        return ()
    }

let deleteAsync (connectionString: string) (customerCode: string) (customerBranch: int) : Task<unit> =
    task {
        let sql = "DELETE FROM 顧客マスタ WHERE 顧客コード = @CustomerCode AND 顧客枝番 = @CustomerBranch"
        use connection = new NpgsqlConnection(connectionString)
        let! _ = connection.ExecuteAsync(sql, {| CustomerCode = customerCode; CustomerBranch = customerBranch |})
        return ()
    }

let findByIdAsync (connectionString: string) (customerCode: string) (customerBranch: int) : Task<Customer option> =
    task {
        let sql = """
            SELECT 顧客コード AS CustomerCode,
                   顧客枝番 AS CustomerBranch,
                   顧客区分 AS CustomerType,
                   請求先コード AS ArCode,
                   請求先枝番 AS ArBranch,
                   回収先コード AS PayerCode,
                   回収先枝番 AS PayerBranch,
                   顧客名 AS CustomerName,
                   顧客名カナ AS CustomerNameKana,
                   自社担当者コード AS EmployeeCode,
                   顧客担当者名 AS CustomerUserName,
                   顧客部門名 AS CustomerDepartmentName,
                   顧客郵便番号 AS CustomerZipCode,
                   顧客都道府県 AS CustomerState,
                   顧客住所１ AS CustomerAddress1,
                   顧客住所２ AS CustomerAddress2,
                   顧客電話番号 AS CustomerTel,
                   顧客ＦＡＸ番号 AS CustomerFax,
                   顧客メールアドレス AS CustomerEmail,
                   顧客請求区分 AS CustomerArType,
                   顧客締日１ AS CustomerCloseDate1,
                   顧客支払月１ AS CustomerPayMonths1,
                   顧客支払日１ AS CustomerPayDates1,
                   顧客支払方法１ AS CustomerPayMethod1,
                   顧客締日２ AS CustomerCloseDate2,
                   顧客支払月２ AS CustomerPayMonths2,
                   顧客支払日２ AS CustomerPayDates2,
                   顧客支払方法２ AS CustomerPayMethod2,
                   作成日時 AS CreatedAt,
                   作成者名 AS CreatedBy,
                   更新日時 AS UpdatedAt,
                   更新者名 AS UpdatedBy
            FROM 顧客マスタ
            WHERE 顧客コード = @CustomerCode AND 顧客枝番 = @CustomerBranch"""
        use connection = new NpgsqlConnection(connectionString)
        let! result = connection.QuerySingleOrDefaultAsync<Customer>(sql, {| CustomerCode = customerCode; CustomerBranch = customerBranch |})
        return if isNull (box result) then None else Some result
    }

let findByCustomerCodeAsync (connectionString: string) (customerCode: string) : Task<seq<Customer>> =
    task {
        let sql = """
            SELECT 顧客コード AS CustomerCode,
                   顧客枝番 AS CustomerBranch,
                   顧客区分 AS CustomerType,
                   請求先コード AS ArCode,
                   請求先枝番 AS ArBranch,
                   回収先コード AS PayerCode,
                   回収先枝番 AS PayerBranch,
                   顧客名 AS CustomerName,
                   顧客名カナ AS CustomerNameKana,
                   自社担当者コード AS EmployeeCode,
                   顧客担当者名 AS CustomerUserName,
                   顧客部門名 AS CustomerDepartmentName,
                   顧客郵便番号 AS CustomerZipCode,
                   顧客都道府県 AS CustomerState,
                   顧客住所１ AS CustomerAddress1,
                   顧客住所２ AS CustomerAddress2,
                   顧客電話番号 AS CustomerTel,
                   顧客ＦＡＸ番号 AS CustomerFax,
                   顧客メールアドレス AS CustomerEmail,
                   顧客請求区分 AS CustomerArType,
                   顧客締日１ AS CustomerCloseDate1,
                   顧客支払月１ AS CustomerPayMonths1,
                   顧客支払日１ AS CustomerPayDates1,
                   顧客支払方法１ AS CustomerPayMethod1,
                   顧客締日２ AS CustomerCloseDate2,
                   顧客支払月２ AS CustomerPayMonths2,
                   顧客支払日２ AS CustomerPayDates2,
                   顧客支払方法２ AS CustomerPayMethod2,
                   作成日時 AS CreatedAt,
                   作成者名 AS CreatedBy,
                   更新日時 AS UpdatedAt,
                   更新者名 AS UpdatedBy
            FROM 顧客マスタ
            WHERE 顧客コード = @CustomerCode
            ORDER BY 顧客枝番"""
        use connection = new NpgsqlConnection(connectionString)
        let! results = connection.QueryAsync<Customer>(sql, {| CustomerCode = customerCode |})
        return results :> seq<Customer>
    }

let findByCompanyCodeAsync (connectionString: string) (companyCode: string) : Task<seq<Customer>> =
    task {
        let sql = """
            SELECT 顧客コード AS CustomerCode,
                   顧客枝番 AS CustomerBranch,
                   顧客区分 AS CustomerType,
                   請求先コード AS ArCode,
                   請求先枝番 AS ArBranch,
                   回収先コード AS PayerCode,
                   回収先枝番 AS PayerBranch,
                   顧客名 AS CustomerName,
                   顧客名カナ AS CustomerNameKana,
                   自社担当者コード AS EmployeeCode,
                   顧客担当者名 AS CustomerUserName,
                   顧客部門名 AS CustomerDepartmentName,
                   顧客郵便番号 AS CustomerZipCode,
                   顧客都道府県 AS CustomerState,
                   顧客住所１ AS CustomerAddress1,
                   顧客住所２ AS CustomerAddress2,
                   顧客電話番号 AS CustomerTel,
                   顧客ＦＡＸ番号 AS CustomerFax,
                   顧客メールアドレス AS CustomerEmail,
                   顧客請求区分 AS CustomerArType,
                   顧客締日１ AS CustomerCloseDate1,
                   顧客支払月１ AS CustomerPayMonths1,
                   顧客支払日１ AS CustomerPayDates1,
                   顧客支払方法１ AS CustomerPayMethod1,
                   顧客締日２ AS CustomerCloseDate2,
                   顧客支払月２ AS CustomerPayMonths2,
                   顧客支払日２ AS CustomerPayDates2,
                   顧客支払方法２ AS CustomerPayMethod2,
                   作成日時 AS CreatedAt,
                   作成者名 AS CreatedBy,
                   更新日時 AS UpdatedAt,
                   更新者名 AS UpdatedBy
            FROM 顧客マスタ
            WHERE 顧客コード = @CompanyCode
            ORDER BY 顧客枝番"""
        use connection = new NpgsqlConnection(connectionString)
        let! results = connection.QueryAsync<Customer>(sql, {| CompanyCode = companyCode |})
        return results :> seq<Customer>
    }
