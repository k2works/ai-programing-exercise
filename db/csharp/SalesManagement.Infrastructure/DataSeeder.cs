using Dapper;
using Npgsql;
using SalesManagement.Domain.Models;

namespace SalesManagement.Infrastructure;

/// <summary>
/// テストデータ投入クラス
/// </summary>
public class DataSeeder
{
    private readonly string _connectionString;

    public DataSeeder(string connectionString)
    {
        _connectionString = connectionString;
    }

    /// <summary>
    /// 全てのSeedデータを投入
    /// </summary>
    public async Task SeedAllAsync()
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.OpenAsync();

        await using var transaction = await connection.BeginTransactionAsync();

        try
        {
            Console.WriteLine("=== Seedデータ投入開始 ===");

            // 既存データのクリア（開発環境のみ）
            await ClearExistingDataAsync(connection, transaction);

            // マスタデータの投入（外部キー制約を考慮した順序）
            await SeedDepartmentsAsync(connection, transaction);
            await SeedEmployeesAsync(connection, transaction);
            await SeedCompanyGroupsAsync(connection, transaction);
            await SeedCompaniesAsync(connection, transaction);
            await SeedCustomersAsync(connection, transaction);
            await SeedSuppliersAsync(connection, transaction);
            await SeedProductCategoriesAsync(connection, transaction);
            await SeedProductsAsync(connection, transaction);
            await SeedWarehousesAsync(connection, transaction);
            await SeedBankAccountsAsync(connection, transaction);

            await transaction.CommitAsync();
            Console.WriteLine("=== Seedデータ投入完了 ===");
        }
        catch (Exception ex)
        {
            await transaction.RollbackAsync();
            Console.WriteLine($"エラー: {ex.Message}");
            throw;
        }
    }

    private async Task ClearExistingDataAsync(NpgsqlConnection connection, NpgsqlTransaction transaction)
    {
        Console.WriteLine("既存データのクリア...");

        // 外部キー制約の逆順で削除
        var clearSql = @"
            -- トランザクションデータ
            DELETE FROM 支払データ;
            DELETE FROM 入金データ;
            DELETE FROM 請求データ明細;
            DELETE FROM 請求データ;
            DELETE FROM 在庫データ;
            DELETE FROM 仕入データ明細;
            DELETE FROM 仕入データ;
            DELETE FROM 発注データ明細;
            DELETE FROM 発注データ;
            DELETE FROM 売上データ明細;
            DELETE FROM 売上データ;
            DELETE FROM 受注データ明細;
            DELETE FROM 受注データ;

            -- 管理系マスタ
            DELETE FROM 与信残高データ;
            DELETE FROM 自動採番マスタ;
            DELETE FROM 入金口座マスタ;

            -- 商品・在庫マスタ
            DELETE FROM 倉庫マスタ;
            DELETE FROM 商品マスタ;
            DELETE FROM 商品分類マスタ;

            -- 取引先マスタ
            DELETE FROM 仕入先マスタ;
            DELETE FROM 顧客マスタ;
            DELETE FROM 取引先マスタ;
            DELETE FROM 取引先グループマスタ;

            -- 組織マスタ
            DELETE FROM 社員マスタ;
            DELETE FROM 部門マスタ;
        ";

        await connection.ExecuteAsync(clearSql, transaction: transaction);
        Console.WriteLine("既存データのクリア完了");
    }

    private async Task SeedDepartmentsAsync(NpgsqlConnection connection, NpgsqlTransaction transaction)
    {
        Console.WriteLine("部門マスタの投入...");

        var now = DateTime.Now;
        var startDate = new DateTime(2020, 4, 1); // 開始日
        var endDate = new DateTime(9999, 12, 31); // 終了日（無期限）

        var departments = new List<object>
        {
            // 本社
            new { Code = "000000", StartDate = startDate, EndDate = endDate, Name = "本社", Path = "/000000", Layer = 1, LowestLayer = (short)0, CanInput = (short)0, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },

            // 食肉製造・販売事業
            new { Code = "100000", StartDate = startDate, EndDate = endDate, Name = "食肉製造・販売事業", Path = "/000000/100000", Layer = 2, LowestLayer = (short)0, CanInput = (short)0, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "110000", StartDate = startDate, EndDate = endDate, Name = "食肉加工部門", Path = "/000000/100000/110000", Layer = 3, LowestLayer = (short)0, CanInput = (short)0, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "111000", StartDate = startDate, EndDate = endDate, Name = "牛肉・豚肉・鶏肉課", Path = "/000000/100000/110000/111000", Layer = 4, LowestLayer = (short)1, CanInput = (short)1, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "112000", StartDate = startDate, EndDate = endDate, Name = "食肉加工品課", Path = "/000000/100000/110000/112000", Layer = 4, LowestLayer = (short)1, CanInput = (short)1, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "120000", StartDate = startDate, EndDate = endDate, Name = "小売販売部門", Path = "/000000/100000/120000", Layer = 3, LowestLayer = (short)0, CanInput = (short)0, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "121000", StartDate = startDate, EndDate = endDate, Name = "直営小売店課", Path = "/000000/100000/120000/121000", Layer = 4, LowestLayer = (short)1, CanInput = (short)1, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "122000", StartDate = startDate, EndDate = endDate, Name = "百貨店・スーパー向け販売課", Path = "/000000/100000/120000/122000", Layer = 4, LowestLayer = (short)1, CanInput = (short)1, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "130000", StartDate = startDate, EndDate = endDate, Name = "新規取引先開拓部門", Path = "/000000/100000/130000", Layer = 3, LowestLayer = (short)0, CanInput = (short)0, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "131000", StartDate = startDate, EndDate = endDate, Name = "ホテル・旅館向け課", Path = "/000000/100000/130000/131000", Layer = 4, LowestLayer = (short)1, CanInput = (short)1, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "132000", StartDate = startDate, EndDate = endDate, Name = "飲食店向け課", Path = "/000000/100000/130000/132000", Layer = 4, LowestLayer = (short)1, CanInput = (short)1, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },

            // 食肉加工品事業
            new { Code = "200000", StartDate = startDate, EndDate = endDate, Name = "食肉加工品事業", Path = "/000000/200000", Layer = 2, LowestLayer = (short)0, CanInput = (short)0, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "210000", StartDate = startDate, EndDate = endDate, Name = "自社ブランド部門", Path = "/000000/200000/210000", Layer = 3, LowestLayer = (short)0, CanInput = (short)0, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "211000", StartDate = startDate, EndDate = endDate, Name = "贈答用製品製造課", Path = "/000000/200000/210000/211000", Layer = 4, LowestLayer = (short)1, CanInput = (short)1, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "212000", StartDate = startDate, EndDate = endDate, Name = "道の駅・土産物製品販売課", Path = "/000000/200000/210000/212000", Layer = 4, LowestLayer = (short)1, CanInput = (short)1, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "220000", StartDate = startDate, EndDate = endDate, Name = "相手先ブランド製造(OEM)部門", Path = "/000000/200000/220000", Layer = 3, LowestLayer = (short)0, CanInput = (short)0, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "221000", StartDate = startDate, EndDate = endDate, Name = "客先要望対応課", Path = "/000000/200000/220000/221000", Layer = 4, LowestLayer = (short)1, CanInput = (short)1, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },

            // コンサルティング事業
            new { Code = "300000", StartDate = startDate, EndDate = endDate, Name = "コンサルティング事業", Path = "/000000/300000", Layer = 2, LowestLayer = (short)0, CanInput = (short)0, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "310000", StartDate = startDate, EndDate = endDate, Name = "顧客対応部門", Path = "/000000/300000/310000", Layer = 3, LowestLayer = (short)0, CanInput = (short)0, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "311000", StartDate = startDate, EndDate = endDate, Name = "メニュー提案課", Path = "/000000/300000/310000/311000", Layer = 4, LowestLayer = (short)1, CanInput = (short)1, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "312000", StartDate = startDate, EndDate = endDate, Name = "半加工商品提供課", Path = "/000000/300000/310000/312000", Layer = 4, LowestLayer = (short)1, CanInput = (short)1, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" }
        };

        var sql = @"
            INSERT INTO 部門マスタ (
                部門コード, 開始日, 終了日, 部門名, 組織階層, 部門パス, 最下層区分, 伝票入力可否,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @Code, @StartDate, @EndDate, @Name, @Layer, @Path, @LowestLayer, @CanInput,
                @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
            )";

        await connection.ExecuteAsync(sql, departments, transaction);
        Console.WriteLine($"部門マスタ: {departments.Count}件投入完了");
    }

    private async Task SeedEmployeesAsync(NpgsqlConnection connection, NpgsqlTransaction transaction)
    {
        Console.WriteLine("社員マスタの投入...");

        var now = DateTime.Now;
        var employees = new List<object>
        {
            // 経営層
            new { Code = "E00001", Name = "山田 太郎", DeptCode = "000000", HireDate = new DateTime(1955, 4, 1), CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "E00002", Name = "佐藤 次郎", DeptCode = "000000", HireDate = new DateTime(1960, 4, 1), CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },

            // 食肉製造・販売事業（正社員）
            new { Code = "E10001", Name = "鈴木 一郎", DeptCode = "111000", HireDate = new DateTime(2010, 4, 1), CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "E10002", Name = "高橋 花子", DeptCode = "112000", HireDate = new DateTime(2012, 4, 1), CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "E10003", Name = "田中 次郎", DeptCode = "121000", HireDate = new DateTime(2015, 4, 1), CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "E10004", Name = "伊藤 三郎", DeptCode = "122000", HireDate = new DateTime(2016, 4, 1), CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "E10005", Name = "渡辺 四郎", DeptCode = "131000", HireDate = new DateTime(2018, 4, 1), CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "E10006", Name = "山本 五郎", DeptCode = "132000", HireDate = new DateTime(2019, 4, 1), CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },

            // 食肉加工品事業（正社員）
            new { Code = "E20001", Name = "中村 太郎", DeptCode = "211000", HireDate = new DateTime(2013, 4, 1), CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "E20002", Name = "小林 次郎", DeptCode = "212000", HireDate = new DateTime(2014, 4, 1), CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "E20003", Name = "加藤 三郎", DeptCode = "221000", HireDate = new DateTime(2017, 4, 1), CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },

            // コンサルティング事業（正社員）
            new { Code = "E30001", Name = "吉田 太郎", DeptCode = "311000", HireDate = new DateTime(2020, 4, 1), CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "E30002", Name = "山口 次郎", DeptCode = "312000", HireDate = new DateTime(2021, 4, 1), CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" }
        };

        var sql = @"
            INSERT INTO 社員マスタ (
                社員コード, 社員名, 部門コード, 入社年月日,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @Code, @Name, @DeptCode, @HireDate,
                @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
            )";

        await connection.ExecuteAsync(sql, employees, transaction);
        Console.WriteLine($"社員マスタ: {employees.Count}件投入完了");
    }

    private async Task SeedCompanyGroupsAsync(NpgsqlConnection connection, NpgsqlTransaction transaction)
    {
        Console.WriteLine("取引先グループの投入...");

        var now = DateTime.Now;
        var groups = new List<object>
        {
            new { Code = "G001", Name = "百貨店", CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "G002", Name = "スーパー", CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "G003", Name = "ホテル・旅館", CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "G004", Name = "飲食店", CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "G005", Name = "観光施設", CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "G006", Name = "食肉卸", CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "G007", Name = "畜産業者", CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" }
        };

        var sql = @"
            INSERT INTO 取引先グループマスタ (
                取引先グループコード, 取引先グループ名,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @Code, @Name, @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
            )";

        await connection.ExecuteAsync(sql, groups, transaction);
        Console.WriteLine($"取引先グループ: {groups.Count}件投入完了");
    }

    private async Task SeedCompaniesAsync(NpgsqlConnection connection, NpgsqlTransaction transaction)
    {
        Console.WriteLine("取引先マスタの投入...");

        var now = DateTime.Now;
        var companies = new List<object>
        {
            // 得意先（顧客）
            new { Code = "C0000001", Name = "X県百貨店", GroupCode = "G001", SupplierFlag = 0, Zip = "1000001", Prefecture = "東京都", Address1 = "X市中央1-1-1", Address2 = "", BanFlag = 0, MiscFlag = 0, CreditLimit = 1000000, CreditTempIncrease = 0, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "C0000002", Name = "Y県百貨店", GroupCode = "G001", SupplierFlag = 0, Zip = "2000002", Prefecture = "神奈川", Address1 = "Y市中央2-2-2", Address2 = "", BanFlag = 0, MiscFlag = 0, CreditLimit = 1000000, CreditTempIncrease = 0, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "C0000003", Name = "地域スーパーA", GroupCode = "G002", SupplierFlag = 0, Zip = "3000003", Prefecture = "東京都", Address1 = "X市東3-3-3", Address2 = "", BanFlag = 0, MiscFlag = 0, CreditLimit = 500000, CreditTempIncrease = 0, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "C0000004", Name = "広域スーパーB", GroupCode = "G002", SupplierFlag = 0, Zip = "4000004", Prefecture = "東京都", Address1 = "X市西4-4-4", Address2 = "", BanFlag = 0, MiscFlag = 0, CreditLimit = 800000, CreditTempIncrease = 0, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "C0000005", Name = "シティホテルC", GroupCode = "G003", SupplierFlag = 0, Zip = "5000005", Prefecture = "東京都", Address1 = "X市南5-5-5", Address2 = "", BanFlag = 0, MiscFlag = 0, CreditLimit = 300000, CreditTempIncrease = 0, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "C0000006", Name = "温泉旅館D", GroupCode = "G003", SupplierFlag = 0, Zip = "6000006", Prefecture = "神奈川", Address1 = "Y市北6-6-6", Address2 = "", BanFlag = 0, MiscFlag = 0, CreditLimit = 200000, CreditTempIncrease = 0, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "C0000007", Name = "焼肉レストランE", GroupCode = "G004", SupplierFlag = 0, Zip = "7000007", Prefecture = "東京都", Address1 = "X市中央7-7-7", Address2 = "", BanFlag = 0, MiscFlag = 0, CreditLimit = 150000, CreditTempIncrease = 0, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "C0000008", Name = "イタリアンF", GroupCode = "G004", SupplierFlag = 0, Zip = "8000008", Prefecture = "東京都", Address1 = "X市東8-8-8", Address2 = "", BanFlag = 0, MiscFlag = 0, CreditLimit = 150000, CreditTempIncrease = 0, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "C0000009", Name = "道の駅G", GroupCode = "G005", SupplierFlag = 0, Zip = "9000009", Prefecture = "千葉県", Address1 = "Z市西9-9-9", Address2 = "", BanFlag = 0, MiscFlag = 0, CreditLimit = 100000, CreditTempIncrease = 0, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "C0000010", Name = "観光センターH", GroupCode = "G005", SupplierFlag = 0, Zip = "1100010", Prefecture = "千葉県", Address1 = "Z市南10-10-10", Address2 = "", BanFlag = 0, MiscFlag = 0, CreditLimit = 100000, CreditTempIncrease = 0, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },

            // 仕入先
            new { Code = "S0000001", Name = "地域食肉卸A", GroupCode = "G006", SupplierFlag = 1, Zip = "1200011", Prefecture = "東京都", Address1 = "X市北11-11-11", Address2 = "", BanFlag = 0, MiscFlag = 0, CreditLimit = 0, CreditTempIncrease = 0, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "S0000002", Name = "地域食肉卸B", GroupCode = "G006", SupplierFlag = 1, Zip = "1300012", Prefecture = "神奈川", Address1 = "Y市中央12-12-12", Address2 = "", BanFlag = 0, MiscFlag = 0, CreditLimit = 0, CreditTempIncrease = 0, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "S0000003", Name = "地域畜産農家", GroupCode = "G007", SupplierFlag = 1, Zip = "1400013", Prefecture = "千葉県", Address1 = "Z市東13-13-13", Address2 = "", BanFlag = 0, MiscFlag = 0, CreditLimit = 0, CreditTempIncrease = 0, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "S0000004", Name = "県内畜産組合", GroupCode = "G007", SupplierFlag = 1, Zip = "1500014", Prefecture = "千葉県", Address1 = "A市西14-14-14", Address2 = "", BanFlag = 0, MiscFlag = 0, CreditLimit = 0, CreditTempIncrease = 0, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" }
        };

        var sql = @"
            INSERT INTO 取引先マスタ (
                取引先コード, 取引先名, 仕入先区分, 取引先グループコード, 郵便番号, 都道府県, 住所１, 住所２,
                取引禁止フラグ, 雑区分, 与信限度額, 与信一時増加枠,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @Code, @Name, @SupplierFlag, @GroupCode, @Zip, @Prefecture, @Address1, @Address2,
                @BanFlag, @MiscFlag, @CreditLimit, @CreditTempIncrease,
                @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
            )";

        await connection.ExecuteAsync(sql, companies, transaction);
        Console.WriteLine($"取引先マスタ: {companies.Count}件投入完了");
    }

    private async Task SeedCustomersAsync(NpgsqlConnection connection, NpgsqlTransaction transaction)
    {
        Console.WriteLine("顧客マスタの投入...");

        var now = DateTime.Now;
        var customers = new List<object>
        {
            // 取引先コードから顧客を登録（C0000001～C0000010）
            new { Code = "C0000001", BranchNo = 0, Category = 0, BillToCode = "C0000001", BillToBranchNo = 0, CollectFromCode = "C0000001", CollectFromBranchNo = 0, Name = "X県百貨店", EmployeeCode = "E00001", BillingCategory = 0, ClosingDay1 = 31, PaymentMonth1 = 1, PaymentDay1 = 31, PaymentMethod1 = 1, ClosingDay2 = 0, PaymentMonth2 = 1, PaymentDay2 = 0, PaymentMethod2 = 1, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "C0000002", BranchNo = 0, Category = 0, BillToCode = "C0000002", BillToBranchNo = 0, CollectFromCode = "C0000002", CollectFromBranchNo = 0, Name = "Y県百貨店", EmployeeCode = "E00001", BillingCategory = 0, ClosingDay1 = 31, PaymentMonth1 = 1, PaymentDay1 = 31, PaymentMethod1 = 1, ClosingDay2 = 0, PaymentMonth2 = 1, PaymentDay2 = 0, PaymentMethod2 = 1, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "C0000003", BranchNo = 0, Category = 0, BillToCode = "C0000003", BillToBranchNo = 0, CollectFromCode = "C0000003", CollectFromBranchNo = 0, Name = "地域スーパーA", EmployeeCode = "E10001", BillingCategory = 0, ClosingDay1 = 31, PaymentMonth1 = 1, PaymentDay1 = 31, PaymentMethod1 = 1, ClosingDay2 = 0, PaymentMonth2 = 1, PaymentDay2 = 0, PaymentMethod2 = 1, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "C0000004", BranchNo = 0, Category = 0, BillToCode = "C0000004", BillToBranchNo = 0, CollectFromCode = "C0000004", CollectFromBranchNo = 0, Name = "広域スーパーB", EmployeeCode = "E10002", BillingCategory = 0, ClosingDay1 = 31, PaymentMonth1 = 1, PaymentDay1 = 31, PaymentMethod1 = 1, ClosingDay2 = 0, PaymentMonth2 = 1, PaymentDay2 = 0, PaymentMethod2 = 1, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "C0000005", BranchNo = 0, Category = 0, BillToCode = "C0000005", BillToBranchNo = 0, CollectFromCode = "C0000005", CollectFromBranchNo = 0, Name = "シティホテルC", EmployeeCode = "E10003", BillingCategory = 0, ClosingDay1 = 31, PaymentMonth1 = 1, PaymentDay1 = 31, PaymentMethod1 = 1, ClosingDay2 = 0, PaymentMonth2 = 1, PaymentDay2 = 0, PaymentMethod2 = 1, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "C0000006", BranchNo = 0, Category = 0, BillToCode = "C0000006", BillToBranchNo = 0, CollectFromCode = "C0000006", CollectFromBranchNo = 0, Name = "温泉旅館D", EmployeeCode = "E10004", BillingCategory = 0, ClosingDay1 = 31, PaymentMonth1 = 1, PaymentDay1 = 31, PaymentMethod1 = 1, ClosingDay2 = 0, PaymentMonth2 = 1, PaymentDay2 = 0, PaymentMethod2 = 1, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "C0000007", BranchNo = 0, Category = 0, BillToCode = "C0000007", BillToBranchNo = 0, CollectFromCode = "C0000007", CollectFromBranchNo = 0, Name = "焼肉レストランE", EmployeeCode = "E10005", BillingCategory = 0, ClosingDay1 = 31, PaymentMonth1 = 1, PaymentDay1 = 31, PaymentMethod1 = 1, ClosingDay2 = 0, PaymentMonth2 = 1, PaymentDay2 = 0, PaymentMethod2 = 1, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "C0000008", BranchNo = 0, Category = 0, BillToCode = "C0000008", BillToBranchNo = 0, CollectFromCode = "C0000008", CollectFromBranchNo = 0, Name = "イタリアンF", EmployeeCode = "E10006", BillingCategory = 0, ClosingDay1 = 31, PaymentMonth1 = 1, PaymentDay1 = 31, PaymentMethod1 = 1, ClosingDay2 = 0, PaymentMonth2 = 1, PaymentDay2 = 0, PaymentMethod2 = 1, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "C0000009", BranchNo = 0, Category = 0, BillToCode = "C0000009", BillToBranchNo = 0, CollectFromCode = "C0000009", CollectFromBranchNo = 0, Name = "道の駅G", EmployeeCode = "E20001", BillingCategory = 0, ClosingDay1 = 31, PaymentMonth1 = 1, PaymentDay1 = 31, PaymentMethod1 = 1, ClosingDay2 = 0, PaymentMonth2 = 1, PaymentDay2 = 0, PaymentMethod2 = 1, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "C0000010", BranchNo = 0, Category = 0, BillToCode = "C0000010", BillToBranchNo = 0, CollectFromCode = "C0000010", CollectFromBranchNo = 0, Name = "観光センターH", EmployeeCode = "E20002", BillingCategory = 0, ClosingDay1 = 31, PaymentMonth1 = 1, PaymentDay1 = 31, PaymentMethod1 = 1, ClosingDay2 = 0, PaymentMonth2 = 1, PaymentDay2 = 0, PaymentMethod2 = 1, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" }
        };

        var sql = @"
            INSERT INTO 顧客マスタ (
                顧客コード, 顧客枝番, 顧客区分, 請求先コード, 請求先枝番, 回収先コード, 回収先枝番,
                顧客名, 自社担当者コード, 顧客請求区分,
                顧客締日１, 顧客支払月１, 顧客支払日１, 顧客支払方法１,
                顧客締日２, 顧客支払月２, 顧客支払日２, 顧客支払方法２,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @Code, @BranchNo, @Category, @BillToCode, @BillToBranchNo, @CollectFromCode, @CollectFromBranchNo,
                @Name, @EmployeeCode, @BillingCategory,
                @ClosingDay1, @PaymentMonth1, @PaymentDay1, @PaymentMethod1,
                @ClosingDay2, @PaymentMonth2, @PaymentDay2, @PaymentMethod2,
                @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
            )";

        await connection.ExecuteAsync(sql, customers, transaction);
        Console.WriteLine($"顧客マスタ: {customers.Count}件投入完了");
    }

    private async Task SeedSuppliersAsync(NpgsqlConnection connection, NpgsqlTransaction transaction)
    {
        Console.WriteLine("仕入先マスタの投入...");

        var now = DateTime.Now;
        var suppliers = new List<object>
        {
            // 取引先コードから仕入先を登録（S0000001～S0000004）
            new { Code = "S0000001", BranchNo = 0, Name = "地域食肉卸A", ClosingDay = 31, PaymentMonth = 1, PaymentDay = 31, PaymentMethod = 1, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "S0000002", BranchNo = 0, Name = "地域食肉卸B", ClosingDay = 31, PaymentMonth = 1, PaymentDay = 31, PaymentMethod = 1, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "S0000003", BranchNo = 0, Name = "地域畜産農家", ClosingDay = 31, PaymentMonth = 1, PaymentDay = 31, PaymentMethod = 1, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "S0000004", BranchNo = 0, Name = "県内畜産組合", ClosingDay = 31, PaymentMonth = 1, PaymentDay = 31, PaymentMethod = 1, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" }
        };

        var sql = @"
            INSERT INTO 仕入先マスタ (
                仕入先コード, 仕入先枝番, 仕入先名, 仕入先締日, 仕入先支払月, 仕入先支払日, 仕入先支払方法,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @Code, @BranchNo, @Name, @ClosingDay, @PaymentMonth, @PaymentDay, @PaymentMethod,
                @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
            )";

        await connection.ExecuteAsync(sql, suppliers, transaction);
        Console.WriteLine($"仕入先マスタ: {suppliers.Count}件投入完了");
    }

    private async Task SeedProductCategoriesAsync(NpgsqlConnection connection, NpgsqlTransaction transaction)
    {
        Console.WriteLine("商品分類マスタの投入...");

        var now = DateTime.Now;
        var categories = new List<object>
        {
            new { Code = "CAT001", Name = "牛肉", Layer = 1, Path = "/CAT001", LowestLayer = (short)1, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "CAT002", Name = "豚肉", Layer = 1, Path = "/CAT002", LowestLayer = (short)1, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "CAT003", Name = "鶏肉", Layer = 1, Path = "/CAT003", LowestLayer = (short)1, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "CAT004", Name = "加工品", Layer = 1, Path = "/CAT004", LowestLayer = (short)1, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" }
        };

        var sql = @"
            INSERT INTO 商品分類マスタ (
                商品分類コード, 商品分類名, 商品分類階層, 商品分類パス, 最下層区分,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @Code, @Name, @Layer, @Path, @LowestLayer,
                @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
            )";

        await connection.ExecuteAsync(sql, categories, transaction);
        Console.WriteLine($"商品分類マスタ: {categories.Count}件投入完了");
    }

    private async Task SeedProductsAsync(NpgsqlConnection connection, NpgsqlTransaction transaction)
    {
        Console.WriteLine("商品マスタの投入...");

        var now = DateTime.Now;
        var products = new List<object>
        {
            // 牛肉製品
            new { Code = "PRD001", FormalName = "黒毛和牛サーロイン", ShortName = "和牛サーロイン", Category = "PRODUCT", CategoryCode = "CAT001", SalesPrice = 5000, PurchasePrice = 3500, CostOfSales = 3500, TaxCategory = 1, MiscCategory = 0, StockControl = 1, StockAllocation = 0, SupplierCode = "S0000001", SupplierBranchNo = 0, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "PRD002", FormalName = "黒毛和牛ロース", ShortName = "和牛ロース", Category = "PRODUCT", CategoryCode = "CAT001", SalesPrice = 4500, PurchasePrice = 3200, CostOfSales = 3200, TaxCategory = 1, MiscCategory = 0, StockControl = 1, StockAllocation = 0, SupplierCode = "S0000001", SupplierBranchNo = 0, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "PRD003", FormalName = "黒毛和牛カルビ", ShortName = "和牛カルビ", Category = "PRODUCT", CategoryCode = "CAT001", SalesPrice = 4000, PurchasePrice = 2800, CostOfSales = 2800, TaxCategory = 1, MiscCategory = 0, StockControl = 1, StockAllocation = 0, SupplierCode = "S0000001", SupplierBranchNo = 0, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "PRD004", FormalName = "黒毛和牛ヒレ", ShortName = "和牛ヒレ", Category = "PRODUCT", CategoryCode = "CAT001", SalesPrice = 5500, PurchasePrice = 4000, CostOfSales = 4000, TaxCategory = 1, MiscCategory = 0, StockControl = 1, StockAllocation = 0, SupplierCode = "S0000001", SupplierBranchNo = 0, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "PRD005", FormalName = "国産牛切り落とし", ShortName = "牛切り落とし", Category = "PRODUCT", CategoryCode = "CAT001", SalesPrice = 2000, PurchasePrice = 1400, CostOfSales = 1400, TaxCategory = 1, MiscCategory = 0, StockControl = 1, StockAllocation = 0, SupplierCode = "S0000001", SupplierBranchNo = 0, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },

            // 豚肉製品
            new { Code = "PRD006", FormalName = "豚ロース", ShortName = "豚ロース", Category = "PRODUCT", CategoryCode = "CAT002", SalesPrice = 1500, PurchasePrice = 1000, CostOfSales = 1000, TaxCategory = 1, MiscCategory = 0, StockControl = 1, StockAllocation = 0, SupplierCode = "S0000002", SupplierBranchNo = 0, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "PRD007", FormalName = "豚バラ", ShortName = "豚バラ", Category = "PRODUCT", CategoryCode = "CAT002", SalesPrice = 1200, PurchasePrice = 800, CostOfSales = 800, TaxCategory = 1, MiscCategory = 0, StockControl = 1, StockAllocation = 0, SupplierCode = "S0000002", SupplierBranchNo = 0, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "PRD008", FormalName = "豚ヒレ", ShortName = "豚ヒレ", Category = "PRODUCT", CategoryCode = "CAT002", SalesPrice = 1800, PurchasePrice = 1200, CostOfSales = 1200, TaxCategory = 1, MiscCategory = 0, StockControl = 1, StockAllocation = 0, SupplierCode = "S0000002", SupplierBranchNo = 0, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "PRD009", FormalName = "豚コマ", ShortName = "豚コマ", Category = "PRODUCT", CategoryCode = "CAT002", SalesPrice = 800, PurchasePrice = 500, CostOfSales = 500, TaxCategory = 1, MiscCategory = 0, StockControl = 1, StockAllocation = 0, SupplierCode = "S0000002", SupplierBranchNo = 0, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "PRD010", FormalName = "豚肩ロース", ShortName = "豚肩ロース", Category = "PRODUCT", CategoryCode = "CAT002", SalesPrice = 1300, PurchasePrice = 900, CostOfSales = 900, TaxCategory = 1, MiscCategory = 0, StockControl = 1, StockAllocation = 0, SupplierCode = "S0000002", SupplierBranchNo = 0, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },

            // 鶏肉製品
            new { Code = "PRD011", FormalName = "鶏もも", ShortName = "鶏もも", Category = "PRODUCT", CategoryCode = "CAT003", SalesPrice = 800, PurchasePrice = 500, CostOfSales = 500, TaxCategory = 1, MiscCategory = 0, StockControl = 1, StockAllocation = 0, SupplierCode = "S0000003", SupplierBranchNo = 0, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "PRD012", FormalName = "鶏むね", ShortName = "鶏むね", Category = "PRODUCT", CategoryCode = "CAT003", SalesPrice = 600, PurchasePrice = 400, CostOfSales = 400, TaxCategory = 1, MiscCategory = 0, StockControl = 1, StockAllocation = 0, SupplierCode = "S0000003", SupplierBranchNo = 0, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "PRD013", FormalName = "手羽先", ShortName = "手羽先", Category = "PRODUCT", CategoryCode = "CAT003", SalesPrice = 500, PurchasePrice = 300, CostOfSales = 300, TaxCategory = 1, MiscCategory = 0, StockControl = 1, StockAllocation = 0, SupplierCode = "S0000003", SupplierBranchNo = 0, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "PRD014", FormalName = "手羽元", ShortName = "手羽元", Category = "PRODUCT", CategoryCode = "CAT003", SalesPrice = 450, PurchasePrice = 280, CostOfSales = 280, TaxCategory = 1, MiscCategory = 0, StockControl = 1, StockAllocation = 0, SupplierCode = "S0000003", SupplierBranchNo = 0, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "PRD015", FormalName = "鶏ささみ", ShortName = "鶏ささみ", Category = "PRODUCT", CategoryCode = "CAT003", SalesPrice = 700, PurchasePrice = 450, CostOfSales = 450, TaxCategory = 1, MiscCategory = 0, StockControl = 1, StockAllocation = 0, SupplierCode = "S0000003", SupplierBranchNo = 0, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },

            // 加工品
            new { Code = "PRD016", FormalName = "ローストビーフ", ShortName = "ローストビーフ", Category = "PRODUCT", CategoryCode = "CAT004", SalesPrice = 3000, PurchasePrice = 2000, CostOfSales = 2000, TaxCategory = 1, MiscCategory = 0, StockControl = 1, StockAllocation = 0, SupplierCode = "S0000001", SupplierBranchNo = 0, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "PRD017", FormalName = "特製ハム", ShortName = "特製ハム", Category = "PRODUCT", CategoryCode = "CAT004", SalesPrice = 1500, PurchasePrice = 1000, CostOfSales = 1000, TaxCategory = 1, MiscCategory = 0, StockControl = 1, StockAllocation = 0, SupplierCode = "S0000002", SupplierBranchNo = 0, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "PRD018", FormalName = "特製ソーセージ", ShortName = "特製ソーセージ", Category = "PRODUCT", CategoryCode = "CAT004", SalesPrice = 1200, PurchasePrice = 800, CostOfSales = 800, TaxCategory = 1, MiscCategory = 0, StockControl = 1, StockAllocation = 0, SupplierCode = "S0000002", SupplierBranchNo = 0, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "PRD019", FormalName = "特製ベーコン", ShortName = "特製ベーコン", Category = "PRODUCT", CategoryCode = "CAT004", SalesPrice = 1800, PurchasePrice = 1200, CostOfSales = 1200, TaxCategory = 1, MiscCategory = 0, StockControl = 1, StockAllocation = 0, SupplierCode = "S0000002", SupplierBranchNo = 0, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "PRD020", FormalName = "手作りコロッケ", ShortName = "手作りコロッケ", Category = "PRODUCT", CategoryCode = "CAT004", SalesPrice = 300, PurchasePrice = 200, CostOfSales = 200, TaxCategory = 1, MiscCategory = 0, StockControl = 1, StockAllocation = 0, SupplierCode = "S0000004", SupplierBranchNo = 0, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" }
        };

        var sql = @"
            INSERT INTO 商品マスタ (
                商品コード, 商品正式名, 商品略称, 商品区分, 商品分類コード,
                販売単価, 仕入単価, 売上原価, 税区分, 雑区分,
                在庫管理対象区分, 在庫引当区分, 仕入先コード, 仕入先枝番,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @Code, @FormalName, @ShortName, @Category, @CategoryCode,
                @SalesPrice, @PurchasePrice, @CostOfSales, @TaxCategory, @MiscCategory,
                @StockControl, @StockAllocation, @SupplierCode, @SupplierBranchNo,
                @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
            )";

        await connection.ExecuteAsync(sql, products, transaction);
        Console.WriteLine($"商品マスタ: {products.Count}件投入完了");
    }

    private async Task SeedWarehousesAsync(NpgsqlConnection connection, NpgsqlTransaction transaction)
    {
        Console.WriteLine("倉庫マスタの投入...");

        var now = DateTime.Now;
        var warehouses = new List<object>
        {
            new { Code = "001", Name = "本社倉庫", Category = 1, Address = "X県X市中央1-1-1", Phone = "03-1111-1111", ManagerCode = "E00001", CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "002", Name = "工場倉庫", Category = 1, Address = "X県X市工業団地2-2-2", Phone = "03-2222-2222", ManagerCode = "E10001", CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" }
        };

        var sql = @"
            INSERT INTO 倉庫マスタ (
                倉庫コード, 倉庫名, 倉庫区分, 住所, 電話番号, 責任者コード,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @Code, @Name, @Category, @Address, @Phone, @ManagerCode,
                @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
            )";

        await connection.ExecuteAsync(sql, warehouses, transaction);
        Console.WriteLine($"倉庫マスタ: {warehouses.Count}件投入完了");
    }

    private async Task SeedBankAccountsAsync(NpgsqlConnection connection, NpgsqlTransaction transaction)
    {
        Console.WriteLine("入金口座マスタの投入...");

        var now = DateTime.Now;
        var bankAccounts = new List<object>
        {
            new { Code = "BA001", Name = "本店売上入金口座", BankName = "みずほ銀行", BranchName = "X県支店", AccountNumber = "1234567", AccountType = 1, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "BA002", Name = "工場売上入金口座", BankName = "三菱UFJ銀行", BranchName = "Y県支店", AccountNumber = "2345678", AccountType = 1, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" }
        };

        var sql = @"
            INSERT INTO 入金口座マスタ (
                口座コード, 口座名, 銀行名, 支店名, 口座番号, 口座種別,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @Code, @Name, @BankName, @BranchName, @AccountNumber, @AccountType,
                @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
            )";

        await connection.ExecuteAsync(sql, bankAccounts, transaction);
        Console.WriteLine($"入金口座マスタ: {bankAccounts.Count}件投入完了");
    }
}
