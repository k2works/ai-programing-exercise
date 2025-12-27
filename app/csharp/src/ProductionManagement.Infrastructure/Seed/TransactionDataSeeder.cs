using Dapper;
using Microsoft.Extensions.Logging;
using Npgsql;

namespace ProductionManagement.Infrastructure.Seed;

/// <summary>
/// トランザクションデータSeeder
/// E精密工業株式会社（架空）のトランザクションデータを投入
/// </summary>
public class TransactionDataSeeder
{
    private readonly ILogger<TransactionDataSeeder> _logger;
    private readonly NpgsqlConnection _connection;

    public TransactionDataSeeder(
        ILogger<TransactionDataSeeder> logger,
        NpgsqlConnection connection)
    {
        _logger = logger;
        _connection = connection;
    }

    public async Task SeedAllAsync(DateOnly effectiveDate)
    {
        await SeedStocksAsync();
        await SeedOrdersAsync();
        await SeedPurchaseOrdersAsync(effectiveDate);
        await SeedWorkOrdersAsync();
        await SeedCompletionRecordsAsync();
        await SeedLaborRecordsAsync();
    }

    private async Task SeedStocksAsync()
    {
        _logger.LogInformation("在庫情報を投入中...");

        var stocks = new[]
        {
            // 材料倉庫
            new { LocationCode = "WH-MAT", ItemCode = "MAT-001", Quantity = 800m, GoodQuantity = 800m },
            new { LocationCode = "WH-MAT", ItemCode = "MAT-002", Quantity = 150m, GoodQuantity = 150m },
            new { LocationCode = "WH-MAT", ItemCode = "MAT-003", Quantity = 450m, GoodQuantity = 450m },
            new { LocationCode = "WH-MAT", ItemCode = "MAT-004", Quantity = 300m, GoodQuantity = 300m },
            new { LocationCode = "WH-MAT", ItemCode = "MAT-010", Quantity = 600m, GoodQuantity = 600m },

            // 部品倉庫
            new { LocationCode = "WH-PART", ItemCode = "PART-001", Quantity = 200m, GoodQuantity = 200m },
            new { LocationCode = "WH-PART", ItemCode = "PART-002", Quantity = 150m, GoodQuantity = 150m },
            new { LocationCode = "WH-PART", ItemCode = "PART-003", Quantity = 80m, GoodQuantity = 80m },
            new { LocationCode = "WH-PART", ItemCode = "PART-004", Quantity = 120m, GoodQuantity = 120m },
            new { LocationCode = "WH-PART", ItemCode = "PART-005", Quantity = 300m, GoodQuantity = 300m },

            // 半製品在庫
            new { LocationCode = "WH-PART", ItemCode = "SEMI-A001", Quantity = 50m, GoodQuantity = 50m },
            new { LocationCode = "WH-PART", ItemCode = "SEMI-B001", Quantity = 30m, GoodQuantity = 30m },
            new { LocationCode = "WH-PART", ItemCode = "SEMI-B002", Quantity = 40m, GoodQuantity = 40m },
            new { LocationCode = "WH-PART", ItemCode = "SEMI-B003", Quantity = 40m, GoodQuantity = 40m },

            // 製品倉庫
            new { LocationCode = "WH-PROD", ItemCode = "PROD-A001", Quantity = 80m, GoodQuantity = 80m },
            new { LocationCode = "WH-PROD", ItemCode = "PROD-B001", Quantity = 45m, GoodQuantity = 45m },
            new { LocationCode = "WH-PROD", ItemCode = "PROD-C001", Quantity = 60m, GoodQuantity = 60m }
        };

        const string sql = """
            INSERT INTO "在庫情報" (
                "場所コード", "品目コード", "在庫数量", "合格数"
            )
            VALUES (
                @LocationCode, @ItemCode, @Quantity, @GoodQuantity
            )
            """;

        foreach (var stock in stocks)
        {
            await _connection.ExecuteAsync(sql, stock);
        }

        _logger.LogInformation("在庫情報 {Count}件 投入完了", stocks.Length);
    }

    private async Task SeedOrdersAsync()
    {
        _logger.LogInformation("オーダ情報を投入中...");

        var orders = new[]
        {
            // 製造オーダ
            new { OrderNumber = "MO-2025-001", OrderType = "製造",
                  ItemCode = "PROD-A001", Quantity = 100m,
                  StartDate = new DateOnly(2025, 1, 15), DueDate = new DateOnly(2025, 1, 31),
                  LocationCode = "WH-PROD", Status = "確定" },
            new { OrderNumber = "MO-2025-002", OrderType = "製造",
                  ItemCode = "PROD-A001", Quantity = 100m,
                  StartDate = new DateOnly(2025, 1, 22), DueDate = new DateOnly(2025, 2, 7),
                  LocationCode = "WH-PROD", Status = "草案" },
            new { OrderNumber = "MO-2025-003", OrderType = "製造",
                  ItemCode = "PROD-B001", Quantity = 50m,
                  StartDate = new DateOnly(2025, 1, 10), DueDate = new DateOnly(2025, 1, 31),
                  LocationCode = "WH-PROD", Status = "確定" },
            new { OrderNumber = "MO-2025-004", OrderType = "製造",
                  ItemCode = "PROD-B001", Quantity = 50m,
                  StartDate = new DateOnly(2025, 1, 17), DueDate = new DateOnly(2025, 2, 7),
                  LocationCode = "WH-PROD", Status = "草案" },
            new { OrderNumber = "MO-2025-005", OrderType = "製造",
                  ItemCode = "SEMI-A001", Quantity = 120m,
                  StartDate = new DateOnly(2025, 1, 8), DueDate = new DateOnly(2025, 1, 28),
                  LocationCode = "WH-PART", Status = "確定" },
            new { OrderNumber = "MO-2025-006", OrderType = "製造",
                  ItemCode = "SEMI-B002", Quantity = 60m,
                  StartDate = new DateOnly(2025, 1, 5), DueDate = new DateOnly(2025, 1, 25),
                  LocationCode = "WH-PART", Status = "確定" },

            // 購買オーダ
            new { OrderNumber = "PO-2025-001", OrderType = "購買",
                  ItemCode = "MAT-001", Quantity = 200m,
                  StartDate = new DateOnly(2025, 1, 6), DueDate = new DateOnly(2025, 1, 20),
                  LocationCode = "WH-MAT", Status = "確定" },
            new { OrderNumber = "PO-2025-002", OrderType = "購買",
                  ItemCode = "MAT-003", Quantity = 150m,
                  StartDate = new DateOnly(2025, 1, 8), DueDate = new DateOnly(2025, 1, 22),
                  LocationCode = "WH-MAT", Status = "確定" },
            new { OrderNumber = "PO-2025-003", OrderType = "購買",
                  ItemCode = "PART-001", Quantity = 100m,
                  StartDate = new DateOnly(2025, 1, 10), DueDate = new DateOnly(2025, 1, 25),
                  LocationCode = "WH-PART", Status = "草案" }
        };

        const string sql = """
            INSERT INTO "オーダ情報" (
                "オーダNO", "オーダ種別", "品目コード",
                "計画数量", "着手予定日", "納期", "場所コード", "ステータス"
            )
            VALUES (
                @OrderNumber, @OrderType::オーダ種別, @ItemCode,
                @Quantity, @StartDate, @DueDate, @LocationCode, @Status::計画ステータス
            )
            """;

        foreach (var order in orders)
        {
            await _connection.ExecuteAsync(sql, order);
        }

        _logger.LogInformation("オーダ情報 {Count}件 投入完了", orders.Length);
    }

    private async Task SeedPurchaseOrdersAsync(DateOnly effectiveDate)
    {
        _logger.LogInformation("発注データを投入中...");

        // 発注1
        await _connection.ExecuteAsync("""
            INSERT INTO "発注データ" ("発注番号", "発注日", "取引先コード", "ステータス")
            VALUES (@OrderNumber, @OrderDate, @SupplierCode, @Status::発注ステータス)
            """,
            new { OrderNumber = "PUR-2025-001", OrderDate = new DateOnly(2025, 1, 10),
                  SupplierCode = "SUP-001", Status = "発注済" });

        await _connection.ExecuteAsync("""
            INSERT INTO "発注明細データ" (
                "発注番号", "発注行番号", "品目コード",
                "発注数量", "発注単価", "受入予定日", "発注金額"
            )
            VALUES (@OrderNumber, @LineNumber, @ItemCode, @Quantity, @UnitPrice, @DueDate, @Amount)
            """,
            new { OrderNumber = "PUR-2025-001", LineNumber = 1,
                  ItemCode = "MAT-001",
                  Quantity = 200m, UnitPrice = 1500m, DueDate = new DateOnly(2025, 1, 20), Amount = 300000m });

        // 発注2
        await _connection.ExecuteAsync("""
            INSERT INTO "発注データ" ("発注番号", "発注日", "取引先コード", "ステータス")
            VALUES (@OrderNumber, @OrderDate, @SupplierCode, @Status::発注ステータス)
            """,
            new { OrderNumber = "PUR-2025-002", OrderDate = new DateOnly(2025, 1, 10),
                  SupplierCode = "SUP-002", Status = "発注済" });

        await _connection.ExecuteAsync("""
            INSERT INTO "発注明細データ" (
                "発注番号", "発注行番号", "品目コード",
                "発注数量", "発注単価", "受入予定日", "発注金額"
            )
            VALUES (@OrderNumber, @LineNumber, @ItemCode, @Quantity, @UnitPrice, @DueDate, @Amount)
            """,
            new { OrderNumber = "PUR-2025-002", LineNumber = 1,
                  ItemCode = "MAT-003",
                  Quantity = 150m, UnitPrice = 2000m, DueDate = new DateOnly(2025, 1, 22), Amount = 300000m });

        // 発注3
        await _connection.ExecuteAsync("""
            INSERT INTO "発注データ" ("発注番号", "発注日", "取引先コード", "ステータス")
            VALUES (@OrderNumber, @OrderDate, @SupplierCode, @Status::発注ステータス)
            """,
            new { OrderNumber = "PUR-2025-003", OrderDate = new DateOnly(2025, 1, 12),
                  SupplierCode = "SUP-004", Status = "発注済" });

        await _connection.ExecuteAsync("""
            INSERT INTO "発注明細データ" (
                "発注番号", "発注行番号", "品目コード",
                "発注数量", "発注単価", "受入予定日", "発注金額"
            )
            VALUES (@OrderNumber, @LineNumber, @ItemCode, @Quantity, @UnitPrice, @DueDate, @Amount)
            """,
            new { OrderNumber = "PUR-2025-003", LineNumber = 1,
                  ItemCode = "PART-001",
                  Quantity = 100m, UnitPrice = 450m, DueDate = new DateOnly(2025, 1, 25), Amount = 45000m });

        await _connection.ExecuteAsync("""
            INSERT INTO "発注明細データ" (
                "発注番号", "発注行番号", "品目コード",
                "発注数量", "発注単価", "受入予定日", "発注金額"
            )
            VALUES (@OrderNumber, @LineNumber, @ItemCode, @Quantity, @UnitPrice, @DueDate, @Amount)
            """,
            new { OrderNumber = "PUR-2025-003", LineNumber = 2,
                  ItemCode = "PART-002",
                  Quantity = 100m, UnitPrice = 80m, DueDate = new DateOnly(2025, 1, 25), Amount = 8000m });

        _logger.LogInformation("発注データ 3件 投入完了");
    }

    private async Task SeedWorkOrdersAsync()
    {
        _logger.LogInformation("作業指示データを投入中...");

        // 作業指示1
        await _connection.ExecuteAsync("""
            INSERT INTO "作業指示データ" (
                "作業指示番号", "オーダ番号", "作業指示日",
                "品目コード", "作業指示数", "場所コード",
                "開始予定日", "完成予定日", "ステータス"
            )
            VALUES (
                @WorkOrderNumber, @OrderNumber, @IssueDate,
                @ItemCode, @Quantity, @LocationCode,
                @StartDate, @EndDate, @Status::作業指示ステータス
            )
            """,
            new { WorkOrderNumber = "WO-2025-001", OrderNumber = "MO-2025-001",
                  IssueDate = new DateOnly(2025, 1, 15),
                  ItemCode = "PROD-A001", Quantity = 100m, LocationCode = "LINE-3",
                  StartDate = new DateOnly(2025, 1, 20), EndDate = new DateOnly(2025, 1, 21),
                  Status = "作業中" });

        await _connection.ExecuteAsync("""
            INSERT INTO "作業指示明細データ" ("作業指示番号", "工順", "工程コード")
            VALUES (@WorkOrderNumber, @Sequence, @ProcessCode)
            """,
            new { WorkOrderNumber = "WO-2025-001", Sequence = 1, ProcessCode = "ASM" });

        await _connection.ExecuteAsync("""
            INSERT INTO "作業指示明細データ" ("作業指示番号", "工順", "工程コード")
            VALUES (@WorkOrderNumber, @Sequence, @ProcessCode)
            """,
            new { WorkOrderNumber = "WO-2025-001", Sequence = 2, ProcessCode = "INS-SHIP" });

        // 作業指示2
        await _connection.ExecuteAsync("""
            INSERT INTO "作業指示データ" (
                "作業指示番号", "オーダ番号", "作業指示日",
                "品目コード", "作業指示数", "場所コード",
                "開始予定日", "完成予定日", "ステータス"
            )
            VALUES (
                @WorkOrderNumber, @OrderNumber, @IssueDate,
                @ItemCode, @Quantity, @LocationCode,
                @StartDate, @EndDate, @Status::作業指示ステータス
            )
            """,
            new { WorkOrderNumber = "WO-2025-002", OrderNumber = "MO-2025-005",
                  IssueDate = new DateOnly(2025, 1, 12),
                  ItemCode = "SEMI-A001", Quantity = 120m, LocationCode = "LINE-1",
                  StartDate = new DateOnly(2025, 1, 15), EndDate = new DateOnly(2025, 1, 23),
                  Status = "完了" });

        var wo2Details = new[]
        {
            new { WorkOrderNumber = "WO-2025-002", Sequence = 1, ProcessCode = "LATHE" },
            new { WorkOrderNumber = "WO-2025-002", Sequence = 2, ProcessCode = "GRIND" },
            new { WorkOrderNumber = "WO-2025-002", Sequence = 3, ProcessCode = "OUT-MEKI" },
            new { WorkOrderNumber = "WO-2025-002", Sequence = 4, ProcessCode = "INS-PROC" }
        };

        foreach (var detail in wo2Details)
        {
            await _connection.ExecuteAsync("""
                INSERT INTO "作業指示明細データ" ("作業指示番号", "工順", "工程コード")
                VALUES (@WorkOrderNumber, @Sequence, @ProcessCode)
                """, detail);
        }

        _logger.LogInformation("作業指示データ 2件 投入完了");
    }

    private async Task SeedCompletionRecordsAsync()
    {
        _logger.LogInformation("完成実績データを投入中...");

        var records = new[]
        {
            new { CompletionNumber = "CR-2025-001", WorkOrderNumber = "WO-2025-002",
                  ItemCode = "SEMI-A001",
                  CompletionDate = new DateOnly(2025, 1, 16),
                  CompletedQuantity = 118m, GoodQuantity = 116m, DefectQuantity = 2m },
            new { CompletionNumber = "CR-2025-002", WorkOrderNumber = "WO-2025-002",
                  ItemCode = "SEMI-A001",
                  CompletionDate = new DateOnly(2025, 1, 17),
                  CompletedQuantity = 2m, GoodQuantity = 2m, DefectQuantity = 0m }
        };

        const string sql = """
            INSERT INTO "完成実績データ" (
                "完成実績番号", "作業指示番号", "品目コード",
                "完成日", "完成数量", "良品数", "不良品数"
            )
            VALUES (
                @CompletionNumber, @WorkOrderNumber, @ItemCode,
                @CompletionDate, @CompletedQuantity, @GoodQuantity, @DefectQuantity
            )
            """;

        foreach (var record in records)
        {
            await _connection.ExecuteAsync(sql, record);
        }

        _logger.LogInformation("完成実績データ {Count}件 投入完了", records.Length);
    }

    private async Task SeedLaborRecordsAsync()
    {
        _logger.LogInformation("工数実績データを投入中...");

        var records = new[]
        {
            new { LaborNumber = "LH-2025-001", WorkOrderNumber = "WO-2025-002",
                  ItemCode = "SEMI-A001", Sequence = 1, ProcessCode = "LATHE",
                  DepartmentCode = "MFG", EmployeeCode = "EMP-001",
                  WorkDate = new DateOnly(2025, 1, 15), Hours = 8.0m },
            new { LaborNumber = "LH-2025-002", WorkOrderNumber = "WO-2025-002",
                  ItemCode = "SEMI-A001", Sequence = 1, ProcessCode = "LATHE",
                  DepartmentCode = "MFG", EmployeeCode = "EMP-001",
                  WorkDate = new DateOnly(2025, 1, 16), Hours = 6.0m },
            new { LaborNumber = "LH-2025-003", WorkOrderNumber = "WO-2025-002",
                  ItemCode = "SEMI-A001", Sequence = 2, ProcessCode = "GRIND",
                  DepartmentCode = "MFG", EmployeeCode = "EMP-002",
                  WorkDate = new DateOnly(2025, 1, 17), Hours = 7.5m }
        };

        const string sql = """
            INSERT INTO "工数実績データ" (
                "工数実績番号", "作業指示番号",
                "品目コード", "工順", "工程コード",
                "部門コード", "担当者コード",
                "作業日", "工数"
            )
            VALUES (
                @LaborNumber, @WorkOrderNumber,
                @ItemCode, @Sequence, @ProcessCode,
                @DepartmentCode, @EmployeeCode,
                @WorkDate, @Hours
            )
            """;

        foreach (var record in records)
        {
            await _connection.ExecuteAsync(sql, record);
        }

        _logger.LogInformation("工数実績データ {Count}件 投入完了", records.Length);
    }
}
