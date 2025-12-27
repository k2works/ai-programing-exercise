using Dapper;
using Microsoft.Extensions.Logging;
using Npgsql;
using ProductionManagement.Infrastructure.Persistence.TypeHandlers;

namespace ProductionManagement.Infrastructure.Seed;

/// <summary>
/// マスタデータSeeder
/// E精密工業株式会社（架空）のマスタデータを投入
/// </summary>
public class MasterDataSeeder
{
    private readonly ILogger<MasterDataSeeder> _logger;
    private readonly NpgsqlConnection _connection;

    static MasterDataSeeder()
    {
        SqlMapper.AddTypeHandler(new DateOnlyTypeHandler());
    }

    public MasterDataSeeder(
        ILogger<MasterDataSeeder> logger,
        NpgsqlConnection connection)
    {
        _logger = logger;
        _connection = connection;
    }

    public async Task SeedAllAsync(DateOnly effectiveDate)
    {
        await SeedUnitsAsync();
        await SeedLocationsAsync();
        await SeedSuppliersAsync(effectiveDate);
        await SeedDepartmentsAsync();
        await SeedProcessesAsync();
        await SeedItemsAsync(effectiveDate);
        await SeedBomsAsync(effectiveDate);
        await SeedRoutingsAsync();
        await SeedEmployeesAsync();
        await SeedUnitPricesAsync(effectiveDate);
        await SeedDefectsAsync();
    }

    private async Task SeedUnitsAsync()
    {
        _logger.LogInformation("単位マスタを投入中...");

        var units = new[]
        {
            new { UnitCode = "PCS", UnitSymbol = "個", UnitName = "個" },
            new { UnitCode = "KG", UnitSymbol = "kg", UnitName = "キログラム" },
            new { UnitCode = "M", UnitSymbol = "m", UnitName = "メートル" },
            new { UnitCode = "SET", UnitSymbol = "set", UnitName = "セット" },
            new { UnitCode = "L", UnitSymbol = "L", UnitName = "リットル" }
        };

        const string sql = """
            INSERT INTO "単位マスタ" ("単位コード", "単位記号", "単位名")
            VALUES (@UnitCode, @UnitSymbol, @UnitName)
            """;

        foreach (var unit in units)
        {
            await _connection.ExecuteAsync(sql, unit);
        }

        _logger.LogInformation("単位マスタ {Count}件 投入完了", units.Length);
    }

    private async Task SeedLocationsAsync()
    {
        _logger.LogInformation("場所マスタを投入中...");

        var locations = new[]
        {
            new { LocationCode = "WH-MAT", LocationName = "材料倉庫", LocationType = "倉庫" },
            new { LocationCode = "WH-PART", LocationName = "部品倉庫", LocationType = "倉庫" },
            new { LocationCode = "WH-PROD", LocationName = "製品倉庫", LocationType = "倉庫" },
            new { LocationCode = "LINE-1", LocationName = "製造ライン1", LocationType = "製造" },
            new { LocationCode = "LINE-2", LocationName = "製造ライン2", LocationType = "製造" },
            new { LocationCode = "LINE-3", LocationName = "組立ライン", LocationType = "製造" },
            new { LocationCode = "INSP-1", LocationName = "検査場", LocationType = "検査" },
            new { LocationCode = "OUT-AREA", LocationName = "外注エリア", LocationType = "外注" }
        };

        const string sql = """
            INSERT INTO "場所マスタ" ("場所コード", "場所名", "場所区分")
            VALUES (@LocationCode, @LocationName, @LocationType::場所区分)
            """;

        foreach (var location in locations)
        {
            await _connection.ExecuteAsync(sql, location);
        }

        _logger.LogInformation("場所マスタ {Count}件 投入完了", locations.Length);
    }

    private async Task SeedSuppliersAsync(DateOnly effectiveDate)
    {
        _logger.LogInformation("取引先マスタを投入中...");

        var suppliers = new[]
        {
            // 仕入先
            new { SupplierCode = "SUP-001", EffectiveDate = effectiveDate,
                  SupplierName = "東京スチール株式会社", SupplierType = "仕入先" },
            new { SupplierCode = "SUP-002", EffectiveDate = effectiveDate,
                  SupplierName = "大阪金属工業", SupplierType = "仕入先" },
            new { SupplierCode = "SUP-003", EffectiveDate = effectiveDate,
                  SupplierName = "名古屋ベアリング", SupplierType = "仕入先" },
            new { SupplierCode = "SUP-004", EffectiveDate = effectiveDate,
                  SupplierName = "横浜部品センター", SupplierType = "仕入先" },
            new { SupplierCode = "SUP-005", EffectiveDate = effectiveDate,
                  SupplierName = "神戸包装資材", SupplierType = "仕入先" },
            // 外注先
            new { SupplierCode = "OUT-001", EffectiveDate = effectiveDate,
                  SupplierName = "メッキ工業所", SupplierType = "外注先" },
            new { SupplierCode = "OUT-002", EffectiveDate = effectiveDate,
                  SupplierName = "熱処理センター", SupplierType = "外注先" },
            // 得意先
            new { SupplierCode = "CUS-001", EffectiveDate = effectiveDate,
                  SupplierName = "機械メーカーA社", SupplierType = "得意先" },
            new { SupplierCode = "CUS-002", EffectiveDate = effectiveDate,
                  SupplierName = "産業機器B社", SupplierType = "得意先" },
            new { SupplierCode = "CUS-003", EffectiveDate = effectiveDate,
                  SupplierName = "精密機械C社", SupplierType = "得意先" }
        };

        const string sql = """
            INSERT INTO "取引先マスタ" ("取引先コード", "適用開始日", "取引先名", "取引先区分")
            VALUES (@SupplierCode, @EffectiveDate, @SupplierName, @SupplierType::取引先区分)
            """;

        foreach (var supplier in suppliers)
        {
            await _connection.ExecuteAsync(sql, supplier);
        }

        _logger.LogInformation("取引先マスタ {Count}件 投入完了", suppliers.Length);
    }

    private async Task SeedDepartmentsAsync()
    {
        _logger.LogInformation("部門マスタを投入中...");

        var departments = new[]
        {
            new { DepartmentCode = "SALES", DepartmentName = "営業部" },
            new { DepartmentCode = "PROD-PLAN", DepartmentName = "生産管理部" },
            new { DepartmentCode = "MFG", DepartmentName = "製造部" },
            new { DepartmentCode = "QUALITY", DepartmentName = "品質管理部" },
            new { DepartmentCode = "PURCHASE", DepartmentName = "購買部" },
            new { DepartmentCode = "WAREHOUSE", DepartmentName = "倉庫部" },
            new { DepartmentCode = "OUTSOURCE", DepartmentName = "外注管理部" }
        };

        const string sql = """
            INSERT INTO "部門マスタ" ("部門コード", "部門名")
            VALUES (@DepartmentCode, @DepartmentName)
            """;

        foreach (var dept in departments)
        {
            await _connection.ExecuteAsync(sql, dept);
        }

        _logger.LogInformation("部門マスタ {Count}件 投入完了", departments.Length);
    }

    private async Task SeedProcessesAsync()
    {
        _logger.LogInformation("工程マスタを投入中...");

        var processes = new[]
        {
            // 切削工程
            new { ProcessCode = "LATHE", ProcessName = "旋盤加工" },
            new { ProcessCode = "MILL", ProcessName = "フライス加工" },
            new { ProcessCode = "GRIND", ProcessName = "研削加工" },
            new { ProcessCode = "HOB", ProcessName = "ホブ切り" },
            new { ProcessCode = "DRILL", ProcessName = "穴あけ加工" },
            // 組立工程
            new { ProcessCode = "ASM", ProcessName = "組立" },
            new { ProcessCode = "FINAL-ASM", ProcessName = "最終組立" },
            // 検査工程
            new { ProcessCode = "INS-PROC", ProcessName = "工程検査" },
            new { ProcessCode = "INS-SHIP", ProcessName = "出荷検査" },
            new { ProcessCode = "INS-RCV", ProcessName = "受入検査" },
            // 外注工程
            new { ProcessCode = "OUT-MEKI", ProcessName = "メッキ処理" },
            new { ProcessCode = "OUT-HEAT", ProcessName = "熱処理" }
        };

        const string sql = """
            INSERT INTO "工程マスタ" ("工程コード", "工程名")
            VALUES (@ProcessCode, @ProcessName)
            """;

        foreach (var process in processes)
        {
            await _connection.ExecuteAsync(sql, process);
        }

        _logger.LogInformation("工程マスタ {Count}件 投入完了", processes.Length);
    }

    private async Task SeedItemsAsync(DateOnly effectiveDate)
    {
        _logger.LogInformation("品目マスタを投入中...");

        var items = new[]
        {
            // 製品
            new { ItemCode = "PROD-A001", EffectiveDate = effectiveDate, ItemName = "精密シャフトA",
                  ItemCategory = "製品", UnitCode = "PCS", LeadTime = 7, SafetyStock = 100m },
            new { ItemCode = "PROD-B001", EffectiveDate = effectiveDate, ItemName = "ギアボックスアセンブリ",
                  ItemCategory = "製品", UnitCode = "PCS", LeadTime = 14, SafetyStock = 50m },
            new { ItemCode = "PROD-C001", EffectiveDate = effectiveDate, ItemName = "精密プレート",
                  ItemCategory = "製品", UnitCode = "PCS", LeadTime = 5, SafetyStock = 80m },

            // 半製品
            new { ItemCode = "SEMI-A001", EffectiveDate = effectiveDate, ItemName = "加工済みシャフト",
                  ItemCategory = "半製品", UnitCode = "PCS", LeadTime = 5, SafetyStock = 120m },
            new { ItemCode = "SEMI-B001", EffectiveDate = effectiveDate, ItemName = "ギアボックス本体",
                  ItemCategory = "半製品", UnitCode = "PCS", LeadTime = 7, SafetyStock = 60m },
            new { ItemCode = "SEMI-B002", EffectiveDate = effectiveDate, ItemName = "駆動ギア",
                  ItemCategory = "半製品", UnitCode = "PCS", LeadTime = 7, SafetyStock = 80m },
            new { ItemCode = "SEMI-B003", EffectiveDate = effectiveDate, ItemName = "従動ギア",
                  ItemCategory = "半製品", UnitCode = "PCS", LeadTime = 7, SafetyStock = 80m },
            new { ItemCode = "SEMI-C001", EffectiveDate = effectiveDate, ItemName = "加工済みプレート",
                  ItemCategory = "半製品", UnitCode = "PCS", LeadTime = 3, SafetyStock = 100m },

            // 部品
            new { ItemCode = "PART-001", EffectiveDate = effectiveDate, ItemName = "ベアリング 6205",
                  ItemCategory = "部品", UnitCode = "PCS", LeadTime = 7, SafetyStock = 100m },
            new { ItemCode = "PART-002", EffectiveDate = effectiveDate, ItemName = "オイルシール φ20",
                  ItemCategory = "部品", UnitCode = "PCS", LeadTime = 7, SafetyStock = 100m },
            new { ItemCode = "PART-003", EffectiveDate = effectiveDate, ItemName = "標準シャフト φ10",
                  ItemCategory = "部品", UnitCode = "PCS", LeadTime = 7, SafetyStock = 50m },
            new { ItemCode = "PART-004", EffectiveDate = effectiveDate, ItemName = "オイルシール φ30",
                  ItemCategory = "部品", UnitCode = "PCS", LeadTime = 7, SafetyStock = 80m },
            new { ItemCode = "PART-005", EffectiveDate = effectiveDate, ItemName = "ボルトセット M6",
                  ItemCategory = "部品", UnitCode = "SET", LeadTime = 3, SafetyStock = 200m },
            new { ItemCode = "PART-006", EffectiveDate = effectiveDate, ItemName = "ワッシャーセット",
                  ItemCategory = "部品", UnitCode = "SET", LeadTime = 3, SafetyStock = 200m },
            new { ItemCode = "PART-007", EffectiveDate = effectiveDate, ItemName = "Oリング φ25",
                  ItemCategory = "部品", UnitCode = "PCS", LeadTime = 7, SafetyStock = 150m },
            new { ItemCode = "PART-008", EffectiveDate = effectiveDate, ItemName = "ピン φ3",
                  ItemCategory = "部品", UnitCode = "PCS", LeadTime = 3, SafetyStock = 500m },

            // 材料
            new { ItemCode = "MAT-001", EffectiveDate = effectiveDate, ItemName = "丸棒材 SUS304 φ20",
                  ItemCategory = "材料", UnitCode = "KG", LeadTime = 14, SafetyStock = 500m },
            new { ItemCode = "MAT-002", EffectiveDate = effectiveDate, ItemName = "アルミダイキャスト素材",
                  ItemCategory = "材料", UnitCode = "PCS", LeadTime = 21, SafetyStock = 100m },
            new { ItemCode = "MAT-003", EffectiveDate = effectiveDate, ItemName = "歯車用素材 SCM415",
                  ItemCategory = "材料", UnitCode = "KG", LeadTime = 14, SafetyStock = 300m },
            new { ItemCode = "MAT-004", EffectiveDate = effectiveDate, ItemName = "鋼板 SS400 t3",
                  ItemCategory = "材料", UnitCode = "KG", LeadTime = 7, SafetyStock = 200m },
            new { ItemCode = "MAT-005", EffectiveDate = effectiveDate, ItemName = "丸棒材 S45C φ15",
                  ItemCategory = "材料", UnitCode = "KG", LeadTime = 14, SafetyStock = 400m },
            new { ItemCode = "MAT-006", EffectiveDate = effectiveDate, ItemName = "真鍮丸棒 C3604 φ10",
                  ItemCategory = "材料", UnitCode = "KG", LeadTime = 14, SafetyStock = 100m },
            new { ItemCode = "MAT-010", EffectiveDate = effectiveDate, ItemName = "包装材セット",
                  ItemCategory = "材料", UnitCode = "SET", LeadTime = 3, SafetyStock = 500m },
            new { ItemCode = "MAT-011", EffectiveDate = effectiveDate, ItemName = "防錆紙",
                  ItemCategory = "材料", UnitCode = "PCS", LeadTime = 3, SafetyStock = 1000m },
            new { ItemCode = "MAT-012", EffectiveDate = effectiveDate, ItemName = "段ボール箱",
                  ItemCategory = "材料", UnitCode = "PCS", LeadTime = 3, SafetyStock = 200m }
        };

        const string sql = """
            INSERT INTO "品目マスタ" (
                "品目コード", "適用開始日", "品名", "品目区分",
                "単位コード", "リードタイム", "安全在庫数"
            )
            VALUES (
                @ItemCode, @EffectiveDate, @ItemName, @ItemCategory::品目区分,
                @UnitCode, @LeadTime, @SafetyStock
            )
            """;

        foreach (var item in items)
        {
            await _connection.ExecuteAsync(sql, item);
        }

        _logger.LogInformation("品目マスタ {Count}件 投入完了", items.Length);
    }

    private async Task SeedBomsAsync(DateOnly effectiveDate)
    {
        _logger.LogInformation("部品構成表を投入中...");

        var boms = new[]
        {
            // 精密シャフトA の構成
            new { ParentItemCode = "PROD-A001", ChildItemCode = "SEMI-A001", EffectiveDate = effectiveDate,
                  BaseQuantity = 1m, RequiredQuantity = 1m, DefectRate = (decimal?)0.02m },
            new { ParentItemCode = "PROD-A001", ChildItemCode = "PART-001", EffectiveDate = effectiveDate,
                  BaseQuantity = 1m, RequiredQuantity = 1m, DefectRate = (decimal?)null },
            new { ParentItemCode = "PROD-A001", ChildItemCode = "PART-002", EffectiveDate = effectiveDate,
                  BaseQuantity = 1m, RequiredQuantity = 1m, DefectRate = (decimal?)null },
            new { ParentItemCode = "PROD-A001", ChildItemCode = "MAT-010", EffectiveDate = effectiveDate,
                  BaseQuantity = 1m, RequiredQuantity = 1m, DefectRate = (decimal?)null },

            // 加工済みシャフト の構成
            new { ParentItemCode = "SEMI-A001", ChildItemCode = "MAT-001", EffectiveDate = effectiveDate,
                  BaseQuantity = 1m, RequiredQuantity = 0.5m, DefectRate = (decimal?)0.05m },

            // ギアボックスアセンブリ の構成
            new { ParentItemCode = "PROD-B001", ChildItemCode = "SEMI-B001", EffectiveDate = effectiveDate,
                  BaseQuantity = 1m, RequiredQuantity = 1m, DefectRate = (decimal?)null },
            new { ParentItemCode = "PROD-B001", ChildItemCode = "SEMI-B002", EffectiveDate = effectiveDate,
                  BaseQuantity = 1m, RequiredQuantity = 1m, DefectRate = (decimal?)null },
            new { ParentItemCode = "PROD-B001", ChildItemCode = "SEMI-B003", EffectiveDate = effectiveDate,
                  BaseQuantity = 1m, RequiredQuantity = 1m, DefectRate = (decimal?)null },
            new { ParentItemCode = "PROD-B001", ChildItemCode = "PART-003", EffectiveDate = effectiveDate,
                  BaseQuantity = 1m, RequiredQuantity = 1m, DefectRate = (decimal?)null },
            new { ParentItemCode = "PROD-B001", ChildItemCode = "PART-001", EffectiveDate = effectiveDate,
                  BaseQuantity = 1m, RequiredQuantity = 2m, DefectRate = (decimal?)null },
            new { ParentItemCode = "PROD-B001", ChildItemCode = "PART-004", EffectiveDate = effectiveDate,
                  BaseQuantity = 1m, RequiredQuantity = 1m, DefectRate = (decimal?)null },
            new { ParentItemCode = "PROD-B001", ChildItemCode = "PART-005", EffectiveDate = effectiveDate,
                  BaseQuantity = 1m, RequiredQuantity = 1m, DefectRate = (decimal?)null },
            new { ParentItemCode = "PROD-B001", ChildItemCode = "MAT-010", EffectiveDate = effectiveDate,
                  BaseQuantity = 1m, RequiredQuantity = 1m, DefectRate = (decimal?)null },

            // ギアボックス本体 の構成
            new { ParentItemCode = "SEMI-B001", ChildItemCode = "MAT-002", EffectiveDate = effectiveDate,
                  BaseQuantity = 1m, RequiredQuantity = 1m, DefectRate = (decimal?)0.03m },

            // 駆動ギア の構成
            new { ParentItemCode = "SEMI-B002", ChildItemCode = "MAT-003", EffectiveDate = effectiveDate,
                  BaseQuantity = 1m, RequiredQuantity = 0.8m, DefectRate = (decimal?)0.05m },

            // 従動ギア の構成
            new { ParentItemCode = "SEMI-B003", ChildItemCode = "MAT-003", EffectiveDate = effectiveDate,
                  BaseQuantity = 1m, RequiredQuantity = 0.6m, DefectRate = (decimal?)0.05m },

            // 精密プレート の構成
            new { ParentItemCode = "PROD-C001", ChildItemCode = "SEMI-C001", EffectiveDate = effectiveDate,
                  BaseQuantity = 1m, RequiredQuantity = 1m, DefectRate = (decimal?)null },
            new { ParentItemCode = "PROD-C001", ChildItemCode = "MAT-010", EffectiveDate = effectiveDate,
                  BaseQuantity = 1m, RequiredQuantity = 1m, DefectRate = (decimal?)null },

            // 加工済みプレート の構成
            new { ParentItemCode = "SEMI-C001", ChildItemCode = "MAT-004", EffectiveDate = effectiveDate,
                  BaseQuantity = 1m, RequiredQuantity = 2.5m, DefectRate = (decimal?)0.03m }
        };

        const string sql = """
            INSERT INTO "部品構成表" (
                "親品目コード", "子品目コード", "適用開始日",
                "基準数量", "必要数量", "不良率"
            )
            VALUES (
                @ParentItemCode, @ChildItemCode, @EffectiveDate,
                @BaseQuantity, @RequiredQuantity, @DefectRate
            )
            """;

        foreach (var bom in boms)
        {
            await _connection.ExecuteAsync(sql, bom);
        }

        _logger.LogInformation("部品構成表 {Count}件 投入完了", boms.Length);
    }

    private async Task SeedRoutingsAsync()
    {
        _logger.LogInformation("工程表を投入中...");

        var routings = new[]
        {
            // 精密シャフトA
            new { ItemCode = "PROD-A001", ProcessCode = "ASM", Sequence = 1 },
            new { ItemCode = "PROD-A001", ProcessCode = "INS-SHIP", Sequence = 2 },

            // 加工済みシャフト
            new { ItemCode = "SEMI-A001", ProcessCode = "LATHE", Sequence = 1 },
            new { ItemCode = "SEMI-A001", ProcessCode = "GRIND", Sequence = 2 },
            new { ItemCode = "SEMI-A001", ProcessCode = "OUT-MEKI", Sequence = 3 },
            new { ItemCode = "SEMI-A001", ProcessCode = "INS-PROC", Sequence = 4 },

            // ギアボックスアセンブリ
            new { ItemCode = "PROD-B001", ProcessCode = "FINAL-ASM", Sequence = 1 },
            new { ItemCode = "PROD-B001", ProcessCode = "INS-SHIP", Sequence = 2 },

            // ギアボックス本体
            new { ItemCode = "SEMI-B001", ProcessCode = "MILL", Sequence = 1 },
            new { ItemCode = "SEMI-B001", ProcessCode = "DRILL", Sequence = 2 },
            new { ItemCode = "SEMI-B001", ProcessCode = "INS-PROC", Sequence = 3 },

            // 駆動ギア
            new { ItemCode = "SEMI-B002", ProcessCode = "LATHE", Sequence = 1 },
            new { ItemCode = "SEMI-B002", ProcessCode = "HOB", Sequence = 2 },
            new { ItemCode = "SEMI-B002", ProcessCode = "OUT-HEAT", Sequence = 3 },
            new { ItemCode = "SEMI-B002", ProcessCode = "INS-PROC", Sequence = 4 },

            // 従動ギア
            new { ItemCode = "SEMI-B003", ProcessCode = "LATHE", Sequence = 1 },
            new { ItemCode = "SEMI-B003", ProcessCode = "HOB", Sequence = 2 },
            new { ItemCode = "SEMI-B003", ProcessCode = "OUT-HEAT", Sequence = 3 },
            new { ItemCode = "SEMI-B003", ProcessCode = "INS-PROC", Sequence = 4 },

            // 精密プレート
            new { ItemCode = "PROD-C001", ProcessCode = "ASM", Sequence = 1 },
            new { ItemCode = "PROD-C001", ProcessCode = "INS-SHIP", Sequence = 2 },

            // 加工済みプレート
            new { ItemCode = "SEMI-C001", ProcessCode = "MILL", Sequence = 1 },
            new { ItemCode = "SEMI-C001", ProcessCode = "DRILL", Sequence = 2 },
            new { ItemCode = "SEMI-C001", ProcessCode = "INS-PROC", Sequence = 3 }
        };

        const string sql = """
            INSERT INTO "工程表" ("品目コード", "工程コード", "工順")
            VALUES (@ItemCode, @ProcessCode, @Sequence)
            """;

        foreach (var routing in routings)
        {
            await _connection.ExecuteAsync(sql, routing);
        }

        _logger.LogInformation("工程表 {Count}件 投入完了", routings.Length);
    }

    private async Task SeedEmployeesAsync()
    {
        _logger.LogInformation("担当者マスタを投入中...");

        var employees = new[]
        {
            new { EmployeeCode = "EMP-001", EmployeeName = "田中 太郎", DepartmentCode = "MFG" },
            new { EmployeeCode = "EMP-002", EmployeeName = "鈴木 一郎", DepartmentCode = "MFG" },
            new { EmployeeCode = "EMP-003", EmployeeName = "佐藤 次郎", DepartmentCode = "MFG" },
            new { EmployeeCode = "EMP-004", EmployeeName = "高橋 三郎", DepartmentCode = "MFG" },
            new { EmployeeCode = "EMP-005", EmployeeName = "伊藤 四郎", DepartmentCode = "MFG" },
            new { EmployeeCode = "EMP-006", EmployeeName = "渡辺 五郎", DepartmentCode = "QUALITY" },
            new { EmployeeCode = "EMP-007", EmployeeName = "山本 花子", DepartmentCode = "QUALITY" },
            new { EmployeeCode = "EMP-008", EmployeeName = "中村 美咲", DepartmentCode = "PROD-PLAN" },
            new { EmployeeCode = "EMP-009", EmployeeName = "小林 健一", DepartmentCode = "PURCHASE" },
            new { EmployeeCode = "EMP-010", EmployeeName = "加藤 正", DepartmentCode = "WAREHOUSE" },
            new { EmployeeCode = "EMP-011", EmployeeName = "吉田 誠", DepartmentCode = "OUTSOURCE" },
            new { EmployeeCode = "EMP-012", EmployeeName = "山田 浩二", DepartmentCode = "SALES" }
        };

        const string sql = """
            INSERT INTO "担当者マスタ" ("担当者コード", "担当者名", "部門コード")
            VALUES (@EmployeeCode, @EmployeeName, @DepartmentCode)
            """;

        foreach (var employee in employees)
        {
            await _connection.ExecuteAsync(sql, employee);
        }

        _logger.LogInformation("担当者マスタ {Count}件 投入完了", employees.Length);
    }

    private async Task SeedUnitPricesAsync(DateOnly effectiveDate)
    {
        _logger.LogInformation("単価マスタを投入中...");

        var unitPrices = new[]
        {
            // 材料の仕入単価
            new { ItemCode = "MAT-001", SupplierCode = "SUP-001", LotSize = 1m, ValidFrom = effectiveDate, UnitPrice = 1500m },
            new { ItemCode = "MAT-002", SupplierCode = "SUP-002", LotSize = 1m, ValidFrom = effectiveDate, UnitPrice = 3500m },
            new { ItemCode = "MAT-003", SupplierCode = "SUP-002", LotSize = 1m, ValidFrom = effectiveDate, UnitPrice = 2000m },
            new { ItemCode = "MAT-004", SupplierCode = "SUP-001", LotSize = 1m, ValidFrom = effectiveDate, UnitPrice = 800m },

            // 部品の仕入単価
            new { ItemCode = "PART-001", SupplierCode = "SUP-004", LotSize = 1m, ValidFrom = effectiveDate, UnitPrice = 450m },
            new { ItemCode = "PART-002", SupplierCode = "SUP-003", LotSize = 1m, ValidFrom = effectiveDate, UnitPrice = 80m },
            new { ItemCode = "PART-003", SupplierCode = "SUP-003", LotSize = 1m, ValidFrom = effectiveDate, UnitPrice = 320m },
            new { ItemCode = "PART-004", SupplierCode = "SUP-003", LotSize = 1m, ValidFrom = effectiveDate, UnitPrice = 120m },
            new { ItemCode = "PART-005", SupplierCode = "SUP-003", LotSize = 1m, ValidFrom = effectiveDate, UnitPrice = 250m },

            // 包装材の仕入単価
            new { ItemCode = "MAT-010", SupplierCode = "SUP-005", LotSize = 1m, ValidFrom = effectiveDate, UnitPrice = 150m },

            // 外注加工単価
            new { ItemCode = "SEMI-A001", SupplierCode = "OUT-001", LotSize = 1m, ValidFrom = effectiveDate, UnitPrice = 500m },
            new { ItemCode = "SEMI-B002", SupplierCode = "OUT-002", LotSize = 1m, ValidFrom = effectiveDate, UnitPrice = 800m },
            new { ItemCode = "SEMI-B003", SupplierCode = "OUT-002", LotSize = 1m, ValidFrom = effectiveDate, UnitPrice = 700m }
        };

        const string sql = """
            INSERT INTO "単価マスタ" (
                "品目コード", "取引先コード", "ロット単位数",
                "使用開始日", "単価"
            )
            VALUES (
                @ItemCode, @SupplierCode, @LotSize,
                @ValidFrom, @UnitPrice
            )
            """;

        foreach (var unitPrice in unitPrices)
        {
            await _connection.ExecuteAsync(sql, unitPrice);
        }

        _logger.LogInformation("単価マスタ {Count}件 投入完了", unitPrices.Length);
    }

    private async Task SeedDefectsAsync()
    {
        _logger.LogInformation("欠点マスタを投入中...");

        var defects = new[]
        {
            new { DefectCode = "DEF-001", DefectName = "寸法不良", DefectCategory = "加工不良" },
            new { DefectCode = "DEF-002", DefectName = "表面傷", DefectCategory = "外観不良" },
            new { DefectCode = "DEF-003", DefectName = "メッキ不良", DefectCategory = "表面処理不良" },
            new { DefectCode = "DEF-004", DefectName = "熱処理不良", DefectCategory = "熱処理不良" },
            new { DefectCode = "DEF-005", DefectName = "組立不良", DefectCategory = "組立不良" },
            new { DefectCode = "DEF-006", DefectName = "材料不良", DefectCategory = "材料不良" }
        };

        const string sql = """
            INSERT INTO "欠点マスタ" ("欠点コード", "欠点名", "欠点区分")
            VALUES (@DefectCode, @DefectName, @DefectCategory)
            """;

        foreach (var defect in defects)
        {
            await _connection.ExecuteAsync(sql, defect);
        }

        _logger.LogInformation("欠点マスタ {Count}件 投入完了", defects.Length);
    }
}
