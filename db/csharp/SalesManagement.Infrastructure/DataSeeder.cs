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
            DELETE FROM 在庫データ;
            DELETE FROM 倉庫マスタ;
            DELETE FROM 商品マスタ;
            DELETE FROM 商品分類マスタ;

            -- 取引先マスタ
            DELETE FROM 取引先分類所属;
            DELETE FROM 取引先分類;
            DELETE FROM 取引先分類種別;
            DELETE FROM 仕入先マスタ;
            DELETE FROM 顧客マスタ;
            DELETE FROM 取引先マスタ;
            DELETE FROM 取引先グループ;

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
        var departments = new List<object>
        {
            // 本社
            new { Code = "000000", Name = "本社", Path = "/000000", Layer = 1, LowestLayer = 0, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },

            // 食肉製造・販売事業
            new { Code = "100000", Name = "食肉製造・販売事業", Path = "/000000/100000", Layer = 2, LowestLayer = 0, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "110000", Name = "食肉加工部門", Path = "/000000/100000/110000", Layer = 3, LowestLayer = 0, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "111000", Name = "牛肉・豚肉・鶏肉課", Path = "/000000/100000/110000/111000", Layer = 4, LowestLayer = 1, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "112000", Name = "食肉加工品課", Path = "/000000/100000/110000/112000", Layer = 4, LowestLayer = 1, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "120000", Name = "小売販売部門", Path = "/000000/100000/120000", Layer = 3, LowestLayer = 0, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "121000", Name = "直営小売店課", Path = "/000000/100000/120000/121000", Layer = 4, LowestLayer = 1, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "122000", Name = "百貨店・スーパー向け販売課", Path = "/000000/100000/120000/122000", Layer = 4, LowestLayer = 1, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "130000", Name = "新規取引先開拓部門", Path = "/000000/100000/130000", Layer = 3, LowestLayer = 0, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "131000", Name = "ホテル・旅館向け課", Path = "/000000/100000/130000/131000", Layer = 4, LowestLayer = 1, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "132000", Name = "飲食店向け課", Path = "/000000/100000/130000/132000", Layer = 4, LowestLayer = 1, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },

            // 食肉加工品事業
            new { Code = "200000", Name = "食肉加工品事業", Path = "/000000/200000", Layer = 2, LowestLayer = 0, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "210000", Name = "自社ブランド部門", Path = "/000000/200000/210000", Layer = 3, LowestLayer = 0, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "211000", Name = "贈答用製品製造課", Path = "/000000/200000/210000/211000", Layer = 4, LowestLayer = 1, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "212000", Name = "道の駅・土産物製品販売課", Path = "/000000/200000/210000/212000", Layer = 4, LowestLayer = 1, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "220000", Name = "相手先ブランド製造(OEM)部門", Path = "/000000/200000/220000", Layer = 3, LowestLayer = 0, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "221000", Name = "客先要望対応課", Path = "/000000/200000/220000/221000", Layer = 4, LowestLayer = 1, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },

            // コンサルティング事業
            new { Code = "300000", Name = "コンサルティング事業", Path = "/000000/300000", Layer = 2, LowestLayer = 0, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "310000", Name = "顧客対応部門", Path = "/000000/300000/310000", Layer = 3, LowestLayer = 0, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "311000", Name = "メニュー提案課", Path = "/000000/300000/310000/311000", Layer = 4, LowestLayer = 1, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "312000", Name = "半加工商品提供課", Path = "/000000/300000/310000/312000", Layer = 4, LowestLayer = 1, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" }
        };

        var sql = @"
            INSERT INTO 部門マスタ (
                部門コード, 部門名, 部門カナ名, パス, 階層, 最下層フラグ,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @Code, @Name, @Name, @Path, @Layer, @LowestLayer,
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
            new { Code = "E00001", LastName = "山田", FirstName = "太郎", DeptCode = "000000", HireDate = new DateTime(1955, 4, 1), CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "E00002", LastName = "佐藤", FirstName = "次郎", DeptCode = "000000", HireDate = new DateTime(1960, 4, 1), CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },

            // 食肉製造・販売事業（正社員）
            new { Code = "E10001", LastName = "鈴木", FirstName = "一郎", DeptCode = "111000", HireDate = new DateTime(2010, 4, 1), CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "E10002", LastName = "高橋", FirstName = "花子", DeptCode = "112000", HireDate = new DateTime(2012, 4, 1), CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "E10003", LastName = "田中", FirstName = "次郎", DeptCode = "121000", HireDate = new DateTime(2015, 4, 1), CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "E10004", LastName = "伊藤", FirstName = "三郎", DeptCode = "122000", HireDate = new DateTime(2016, 4, 1), CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "E10005", LastName = "渡辺", FirstName = "四郎", DeptCode = "131000", HireDate = new DateTime(2018, 4, 1), CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "E10006", LastName = "山本", FirstName = "五郎", DeptCode = "132000", HireDate = new DateTime(2019, 4, 1), CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },

            // 食肉加工品事業（正社員）
            new { Code = "E20001", LastName = "中村", FirstName = "太郎", DeptCode = "211000", HireDate = new DateTime(2013, 4, 1), CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "E20002", LastName = "小林", FirstName = "次郎", DeptCode = "212000", HireDate = new DateTime(2014, 4, 1), CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "E20003", LastName = "加藤", FirstName = "三郎", DeptCode = "221000", HireDate = new DateTime(2017, 4, 1), CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },

            // コンサルティング事業（正社員）
            new { Code = "E30001", LastName = "吉田", FirstName = "太郎", DeptCode = "311000", HireDate = new DateTime(2020, 4, 1), CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "E30002", LastName = "山口", FirstName = "次郎", DeptCode = "312000", HireDate = new DateTime(2021, 4, 1), CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" }
        };

        var sql = @"
            INSERT INTO 社員マスタ (
                社員コード, 姓, 名, 部門コード, 入社日,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @Code, @LastName, @FirstName, @DeptCode, @HireDate,
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
            new { Code = "GRP001", Name = "百貨店", CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "GRP002", Name = "スーパー", CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "GRP003", Name = "ホテル・旅館", CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "GRP004", Name = "飲食店", CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "GRP005", Name = "観光施設", CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "GRP006", Name = "食肉卸", CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "GRP007", Name = "畜産業者", CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" }
        };

        var sql = @"
            INSERT INTO 取引先グループ (
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
            new { Code = "C0000001", Name = "X県百貨店", GroupCode = "GRP001", Zip = "100-0001", Address = "X県X市中央1-1-1", Phone = "03-1111-1111", CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "C0000002", Name = "Y県百貨店", GroupCode = "GRP001", Zip = "200-0002", Address = "Y県Y市中央2-2-2", Phone = "03-2222-2222", CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "C0000003", Name = "地域スーパーA", GroupCode = "GRP002", Zip = "300-0003", Address = "X県X市東3-3-3", Phone = "03-3333-3333", CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "C0000004", Name = "広域スーパーB", GroupCode = "GRP002", Zip = "400-0004", Address = "X県X市西4-4-4", Phone = "03-4444-4444", CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "C0000005", Name = "シティホテルC", GroupCode = "GRP003", Zip = "500-0005", Address = "X県X市南5-5-5", Phone = "03-5555-5555", CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "C0000006", Name = "温泉旅館D", GroupCode = "GRP003", Zip = "600-0006", Address = "X県Y市北6-6-6", Phone = "03-6666-6666", CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "C0000007", Name = "焼肉レストランE", GroupCode = "GRP004", Zip = "700-0007", Address = "X県X市中央7-7-7", Phone = "03-7777-7777", CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "C0000008", Name = "イタリアンF", GroupCode = "GRP004", Zip = "800-0008", Address = "X県X市東8-8-8", Phone = "03-8888-8888", CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "C0000009", Name = "道の駅G", GroupCode = "GRP005", Zip = "900-0009", Address = "X県Z市西9-9-9", Phone = "03-9999-9999", CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "C0000010", Name = "観光センターH", GroupCode = "GRP005", Zip = "110-0010", Address = "X県Z市南10-10-10", Phone = "03-1010-1010", CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },

            // 仕入先
            new { Code = "S0000001", Name = "地域食肉卸A", GroupCode = "GRP006", Zip = "120-0011", Address = "X県X市北11-11-11", Phone = "03-1111-0001", CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "S0000002", Name = "地域食肉卸B", GroupCode = "GRP006", Zip = "130-0012", Address = "X県Y市中央12-12-12", Phone = "03-1111-0002", CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "S0000003", Name = "地域畜産農家", GroupCode = "GRP007", Zip = "140-0013", Address = "X県Z市東13-13-13", Phone = "03-1111-0003", CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "S0000004", Name = "県内畜産組合", GroupCode = "GRP007", Zip = "150-0014", Address = "X県A市西14-14-14", Phone = "03-1111-0004", CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" }
        };

        var sql = @"
            INSERT INTO 取引先マスタ (
                取引先コード, 取引先名, 取引先グループコード, 郵便番号, 住所, 電話番号,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @Code, @Name, @GroupCode, @Zip, @Address, @Phone,
                @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
            )";

        await connection.ExecuteAsync(sql, companies, transaction);
        Console.WriteLine($"取引先マスタ: {companies.Count}件投入完了");
    }

    private async Task SeedCustomersAsync(NpgsqlConnection connection, NpgsqlTransaction transaction)
    {
        Console.WriteLine("顧客マスタの投入...");

        var now = DateTime.Now;
        var customers = new List<object>();

        // 得意先を顧客として登録（C0000001～C0000010）
        for (int i = 1; i <= 10; i++)
        {
            customers.Add(new
            {
                Code = $"C000000{i}",
                CreditLimit = 10000000m, // 1000万円の与信枠
                CreatedAt = now,
                CreatedBy = "SYSTEM",
                UpdatedAt = now,
                UpdatedBy = "SYSTEM"
            });
        }

        var sql = @"
            INSERT INTO 顧客マスタ (
                得意先コード, 与信限度額,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @Code, @CreditLimit, @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
            )";

        await connection.ExecuteAsync(sql, customers, transaction);
        Console.WriteLine($"顧客マスタ: {customers.Count}件投入完了");
    }

    private async Task SeedSuppliersAsync(NpgsqlConnection connection, NpgsqlTransaction transaction)
    {
        Console.WriteLine("仕入先マスタの投入...");

        var now = DateTime.Now;
        var suppliers = new List<object>();

        // 仕入先として登録（S0000001～S0000004）
        for (int i = 1; i <= 4; i++)
        {
            suppliers.Add(new
            {
                Code = $"S000000{i}",
                PaymentTerms = 30, // 30日後払い
                CreatedAt = now,
                CreatedBy = "SYSTEM",
                UpdatedAt = now,
                UpdatedBy = "SYSTEM"
            });
        }

        var sql = @"
            INSERT INTO 仕入先マスタ (
                仕入先コード, 支払条件,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @Code, @PaymentTerms, @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
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
            new { Code = "CAT001", Name = "牛肉", CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "CAT002", Name = "豚肉", CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "CAT003", Name = "鶏肉", CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "CAT004", Name = "加工品", CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" }
        };

        var sql = @"
            INSERT INTO 商品分類マスタ (
                商品分類コード, 商品分類名,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @Code, @Name, @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
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
            new { Code = "PRD001", Name = "黒毛和牛サーロイン", CategoryCode = "CAT001", UnitPrice = 5000m, SupplierCode = "S0000001", CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "PRD002", Name = "黒毛和牛ロース", CategoryCode = "CAT001", UnitPrice = 4500m, SupplierCode = "S0000001", CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "PRD003", Name = "黒毛和牛カルビ", CategoryCode = "CAT001", UnitPrice = 4000m, SupplierCode = "S0000001", CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "PRD004", Name = "黒毛和牛ヒレ", CategoryCode = "CAT001", UnitPrice = 5500m, SupplierCode = "S0000001", CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "PRD005", Name = "国産牛切り落とし", CategoryCode = "CAT001", UnitPrice = 2000m, SupplierCode = "S0000001", CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },

            // 豚肉製品
            new { Code = "PRD006", Name = "豚ロース", CategoryCode = "CAT002", UnitPrice = 1500m, SupplierCode = "S0000002", CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "PRD007", Name = "豚バラ", CategoryCode = "CAT002", UnitPrice = 1200m, SupplierCode = "S0000002", CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "PRD008", Name = "豚ヒレ", CategoryCode = "CAT002", UnitPrice = 1800m, SupplierCode = "S0000002", CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "PRD009", Name = "豚コマ", CategoryCode = "CAT002", UnitPrice = 800m, SupplierCode = "S0000002", CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "PRD010", Name = "豚肩ロース", CategoryCode = "CAT002", UnitPrice = 1300m, SupplierCode = "S0000002", CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },

            // 鶏肉製品
            new { Code = "PRD011", Name = "鶏もも", CategoryCode = "CAT003", UnitPrice = 800m, SupplierCode = "S0000003", CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "PRD012", Name = "鶏むね", CategoryCode = "CAT003", UnitPrice = 600m, SupplierCode = "S0000003", CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "PRD013", Name = "手羽先", CategoryCode = "CAT003", UnitPrice = 500m, SupplierCode = "S0000003", CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "PRD014", Name = "手羽元", CategoryCode = "CAT003", UnitPrice = 450m, SupplierCode = "S0000003", CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "PRD015", Name = "鶏ささみ", CategoryCode = "CAT003", UnitPrice = 700m, SupplierCode = "S0000003", CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },

            // 加工品
            new { Code = "PRD016", Name = "ローストビーフ", CategoryCode = "CAT004", UnitPrice = 3000m, SupplierCode = "S0000001", CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "PRD017", Name = "特製ハム", CategoryCode = "CAT004", UnitPrice = 1500m, SupplierCode = "S0000002", CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "PRD018", Name = "特製ソーセージ", CategoryCode = "CAT004", UnitPrice = 1200m, SupplierCode = "S0000002", CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "PRD019", Name = "特製ベーコン", CategoryCode = "CAT004", UnitPrice = 1800m, SupplierCode = "S0000002", CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "PRD020", Name = "手作りコロッケ", CategoryCode = "CAT004", UnitPrice = 300m, SupplierCode = (string?)null, CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" }
        };

        var sql = @"
            INSERT INTO 商品マスタ (
                商品コード, 商品名, 商品分類コード, 標準単価, 仕入先コード,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @Code, @Name, @CategoryCode, @UnitPrice, @SupplierCode,
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
            new { Code = "WH001", Name = "本社倉庫", Location = "X県X市中央1-1-1", CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" },
            new { Code = "WH002", Name = "工場倉庫", Location = "X県X市工業団地2-2-2", CreatedAt = now, CreatedBy = "SYSTEM", UpdatedAt = now, UpdatedBy = "SYSTEM" }
        };

        var sql = @"
            INSERT INTO 倉庫マスタ (
                倉庫コード, 倉庫名, 所在地,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @Code, @Name, @Location, @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
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
