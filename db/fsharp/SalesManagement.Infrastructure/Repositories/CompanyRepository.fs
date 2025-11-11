module SalesManagement.Infrastructure.Repositories.CompanyRepository

open System.Threading.Tasks
open Npgsql
open Dapper
open SalesManagement.Domain.Models

let insertAsync (connectionString: string) (company: Company) : Task<unit> =
    task {
        let sql = """
            INSERT INTO 取引先マスタ (
                取引先コード, 取引先名, 取引先名カナ, 仕入先区分,
                郵便番号, 都道府県, 住所１, 住所２,
                取引禁止フラグ, 雑区分, 取引先グループコード,
                与信限度額, 与信一時増加枠,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @CompanyCode, @CompanyName, @CompanyNameKana, @SupplierType,
                @ZipCode, @State, @Address1, @Address2,
                @NoSalesFlag, @WideUseType, @CompanyGroupCode,
                @MaxCredit, @TempCreditUp,
                @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
            )"""
        use connection = new NpgsqlConnection(connectionString)
        let! _ = connection.ExecuteAsync(sql, company)
        return ()
    }

let updateAsync (connectionString: string) (company: Company) : Task<unit> =
    task {
        let sql = """
            UPDATE 取引先マスタ
            SET 取引先名 = @CompanyName,
                取引先名カナ = @CompanyNameKana,
                仕入先区分 = @SupplierType,
                郵便番号 = @ZipCode,
                都道府県 = @State,
                住所１ = @Address1,
                住所２ = @Address2,
                取引禁止フラグ = @NoSalesFlag,
                雑区分 = @WideUseType,
                取引先グループコード = @CompanyGroupCode,
                与信限度額 = @MaxCredit,
                与信一時増加枠 = @TempCreditUp,
                更新日時 = @UpdatedAt,
                更新者名 = @UpdatedBy
            WHERE 取引先コード = @CompanyCode"""
        use connection = new NpgsqlConnection(connectionString)
        let! _ = connection.ExecuteAsync(sql, company)
        return ()
    }

let deleteAsync (connectionString: string) (companyCode: string) : Task<unit> =
    task {
        let sql = "DELETE FROM 取引先マスタ WHERE 取引先コード = @CompanyCode"
        use connection = new NpgsqlConnection(connectionString)
        let! _ = connection.ExecuteAsync(sql, {| CompanyCode = companyCode |})
        return ()
    }

let findByIdAsync (connectionString: string) (companyCode: string) : Task<Company option> =
    task {
        let sql = """
            SELECT 取引先コード AS CompanyCode,
                   取引先名 AS CompanyName,
                   取引先名カナ AS CompanyNameKana,
                   仕入先区分 AS SupplierType,
                   郵便番号 AS ZipCode,
                   都道府県 AS State,
                   住所１ AS Address1,
                   住所２ AS Address2,
                   取引禁止フラグ AS NoSalesFlag,
                   雑区分 AS WideUseType,
                   取引先グループコード AS CompanyGroupCode,
                   与信限度額 AS MaxCredit,
                   与信一時増加枠 AS TempCreditUp,
                   作成日時 AS CreatedAt,
                   作成者名 AS CreatedBy,
                   更新日時 AS UpdatedAt,
                   更新者名 AS UpdatedBy
            FROM 取引先マスタ
            WHERE 取引先コード = @CompanyCode"""
        use connection = new NpgsqlConnection(connectionString)
        let! result = connection.QuerySingleOrDefaultAsync<Company>(sql, {| CompanyCode = companyCode |})
        return if isNull (box result) then None else Some result
    }

let findAllAsync (connectionString: string) : Task<seq<Company>> =
    task {
        let sql = """
            SELECT 取引先コード AS CompanyCode,
                   取引先名 AS CompanyName,
                   取引先名カナ AS CompanyNameKana,
                   仕入先区分 AS SupplierType,
                   郵便番号 AS ZipCode,
                   都道府県 AS State,
                   住所１ AS Address1,
                   住所２ AS Address2,
                   取引禁止フラグ AS NoSalesFlag,
                   雑区分 AS WideUseType,
                   取引先グループコード AS CompanyGroupCode,
                   与信限度額 AS MaxCredit,
                   与信一時増加枠 AS TempCreditUp,
                   作成日時 AS CreatedAt,
                   作成者名 AS CreatedBy,
                   更新日時 AS UpdatedAt,
                   更新者名 AS UpdatedBy
            FROM 取引先マスタ
            ORDER BY 取引先コード"""
        use connection = new NpgsqlConnection(connectionString)
        let! results = connection.QueryAsync<Company>(sql)
        return results :> seq<Company>
    }

let findByGroupCodeAsync (connectionString: string) (companyGroupCode: string) : Task<seq<Company>> =
    task {
        let sql = """
            SELECT 取引先コード AS CompanyCode,
                   取引先名 AS CompanyName,
                   取引先名カナ AS CompanyNameKana,
                   仕入先区分 AS SupplierType,
                   郵便番号 AS ZipCode,
                   都道府県 AS State,
                   住所１ AS Address1,
                   住所２ AS Address2,
                   取引禁止フラグ AS NoSalesFlag,
                   雑区分 AS WideUseType,
                   取引先グループコード AS CompanyGroupCode,
                   与信限度額 AS MaxCredit,
                   与信一時増加枠 AS TempCreditUp,
                   作成日時 AS CreatedAt,
                   作成者名 AS CreatedBy,
                   更新日時 AS UpdatedAt,
                   更新者名 AS UpdatedBy
            FROM 取引先マスタ
            WHERE 取引先グループコード = @CompanyGroupCode
            ORDER BY 取引先コード"""
        use connection = new NpgsqlConnection(connectionString)
        let! results = connection.QueryAsync<Company>(sql, {| CompanyGroupCode = companyGroupCode |})
        return results :> seq<Company>
    }
