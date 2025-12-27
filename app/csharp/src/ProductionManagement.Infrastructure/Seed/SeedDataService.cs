using Dapper;
using Microsoft.Extensions.Logging;
using Npgsql;

namespace ProductionManagement.Infrastructure.Seed;

/// <summary>
/// Seedデータ投入サービス
/// </summary>
public class SeedDataService
{
    private readonly ILogger<SeedDataService> _logger;
    private readonly NpgsqlConnection _connection;
    private readonly MasterDataSeeder _masterDataSeeder;
    private readonly TransactionDataSeeder _transactionDataSeeder;

    public SeedDataService(
        ILogger<SeedDataService> logger,
        NpgsqlConnection connection,
        MasterDataSeeder masterDataSeeder,
        TransactionDataSeeder transactionDataSeeder)
    {
        _logger = logger;
        _connection = connection;
        _masterDataSeeder = masterDataSeeder;
        _transactionDataSeeder = transactionDataSeeder;
    }

    public async Task SeedAllAsync()
    {
        _logger.LogInformation("========================================");
        _logger.LogInformation("生産管理システム Seed データ投入開始");
        _logger.LogInformation("========================================");

        var effectiveDate = new DateOnly(2025, 1, 1);

        // 既存データの削除
        await CleanAllDataAsync();

        // マスタデータの投入
        await _masterDataSeeder.SeedAllAsync(effectiveDate);

        // トランザクションデータの投入
        await _transactionDataSeeder.SeedAllAsync(effectiveDate);

        _logger.LogInformation("========================================");
        _logger.LogInformation("生産管理システム Seed データ投入完了!");
        _logger.LogInformation("========================================");
    }

    private async Task CleanAllDataAsync()
    {
        _logger.LogInformation("既存データを削除中...");

        // TRUNCATEはFKがある場合にエラーになるのでDELETEを使用
        // テーブルが存在しない場合はスキップ

        // トランザクションデータから削除（外部キー制約のため逆順）
        // 原価データ
        await TryDeleteAsync("原価差異データ");
        await TryDeleteAsync("実際原価データ");
        await TryDeleteAsync("標準原価マスタ");

        // 在庫関連
        await TryDeleteAsync("棚卸差異データ");
        await TryDeleteAsync("棚卸データ");
        await TryDeleteAsync("在庫調整データ");
        await TryDeleteAsync("払出明細データ");
        await TryDeleteAsync("払出データ");

        // 品質関連
        await TryDeleteAsync("出荷検査結果データ");
        await TryDeleteAsync("ロットデータ");
        await TryDeleteAsync("完成検査結果データ");

        // 製造関連
        await TryDeleteAsync("工数実績データ");
        await TryDeleteAsync("完成実績データ");
        await TryDeleteAsync("作業指示明細データ");
        await TryDeleteAsync("作業指示データ");

        // 供給・消費（購買より先に削除: fk_消費_入荷）
        await TryDeleteAsync("消費明細データ");
        await TryDeleteAsync("消費データ");
        await TryDeleteAsync("供給明細データ");
        await TryDeleteAsync("供給データ");

        // 購買関連
        await TryDeleteAsync("欠陥データ");
        await TryDeleteAsync("検収データ");
        await TryDeleteAsync("受入検査データ");
        await TryDeleteAsync("入荷受入データ");
        await TryDeleteAsync("入荷データ");
        await TryDeleteAsync("支給明細データ");
        await TryDeleteAsync("支給データ");
        await TryDeleteAsync("発注明細データ");
        await TryDeleteAsync("発注データ");

        // 計画
        await TryDeleteAsync("引当情報");
        await TryDeleteAsync("所要情報");
        await TryDeleteAsync("オーダ情報");
        await TryDeleteAsync("基準生産計画");

        // 在庫
        await TryDeleteAsync("在庫情報");

        // マスタデータを削除
        await TryDeleteAsync("欠点マスタ");
        await TryDeleteAsync("単価マスタ");
        await TryDeleteAsync("工程表");
        await TryDeleteAsync("部品構成表");
        await TryDeleteAsync("担当者マスタ");
        await TryDeleteAsync("部門マスタ");
        await TryDeleteAsync("工程マスタ");
        await TryDeleteAsync("品目マスタ");
        await TryDeleteAsync("取引先マスタ");
        await TryDeleteAsync("場所マスタ");
        await TryDeleteAsync("単位マスタ");
        await TryDeleteAsync("カレンダマスタ");

        _logger.LogInformation("既存データ削除完了");
    }

    private async Task TryDeleteAsync(string tableName)
    {
        var tableExists = await _connection.ExecuteScalarAsync<bool>(
            "SELECT EXISTS(SELECT FROM information_schema.tables WHERE table_name = @TableName)",
            new { TableName = tableName });

        if (tableExists)
        {
            await _connection.ExecuteAsync($"DELETE FROM \"{tableName}\"");
        }
    }
}
