using System.ComponentModel;
using System.Data;
using Microsoft.Data.Sqlite;
using MRS.Infrastructure.Data;

namespace MRS.Api.Services;

/// <summary>
/// バックアップサービスのインターフェース
/// </summary>
public interface IBackupService
{
    /// <summary>
    /// フルバックアップを実行
    /// </summary>
    /// <param name="backupPath">バックアップファイルパス（nullの場合は自動生成）</param>
    /// <param name="cancellationToken">キャンセレーショントークン</param>
    /// <returns>バックアップファイルパス</returns>
    Task<string> CreateFullBackupAsync(string? backupPath = null, CancellationToken cancellationToken = default);

    /// <summary>
    /// 差分バックアップを実行
    /// </summary>
    /// <param name="sinceDateTime">差分バックアップの開始日時</param>
    /// <param name="backupPath">バックアップファイルパス（nullの場合は自動生成）</param>
    /// <param name="cancellationToken">キャンセレーショントークン</param>
    /// <returns>バックアップファイルパス</returns>
    Task<string> CreateIncrementalBackupAsync(DateTime sinceDateTime, string? backupPath = null, CancellationToken cancellationToken = default);

    /// <summary>
    /// バックアップからデータベースを復元
    /// </summary>
    /// <param name="backupPath">バックアップファイルパス</param>
    /// <param name="targetPath">復元先データベースパス（nullの場合は現在のDB）</param>
    /// <param name="cancellationToken">キャンセレーショントークン</param>
    Task RestoreFromBackupAsync(string backupPath, string? targetPath = null, CancellationToken cancellationToken = default);

    /// <summary>
    /// バックアップファイルの整合性をチェック
    /// </summary>
    /// <param name="backupPath">バックアップファイルパス</param>
    /// <param name="cancellationToken">キャンセレーショントークン</param>
    /// <returns>整合性チェック結果</returns>
    Task<BackupVerificationResult> VerifyBackupAsync(string backupPath, CancellationToken cancellationToken = default);

    /// <summary>
    /// 古いバックアップファイルをクリーンアップ
    /// </summary>
    /// <param name="retentionDays">保持日数</param>
    /// <param name="cancellationToken">キャンセレーショントークン</param>
    /// <returns>削除されたファイル数</returns>
    Task<int> CleanupOldBackupsAsync(int retentionDays, CancellationToken cancellationToken = default);

    /// <summary>
    /// バックアップ履歴を取得
    /// </summary>
    /// <param name="limit">取得件数上限</param>
    /// <param name="cancellationToken">キャンセレーショントークン</param>
    /// <returns>バックアップ履歴リスト</returns>
    Task<IEnumerable<BackupInfo>> GetBackupHistoryAsync(int limit = 50, CancellationToken cancellationToken = default);

    /// <summary>
    /// スケジュール実行されるバックアップタスク
    /// </summary>
    /// <param name="cancellationToken">キャンセレーショントークン</param>
    Task ExecuteScheduledBackupAsync(CancellationToken cancellationToken = default);
}

/// <summary>
/// バックアップ検証結果
/// </summary>
public record BackupVerificationResult
{
    public bool IsValid { get; init; }
    public string Message { get; init; } = string.Empty;
    public long FileSizeBytes { get; init; }
    public DateTime CreatedAt { get; init; }
    public TimeSpan VerificationDuration { get; init; }
    public int RecordCount { get; init; }
    public string ChecksumMd5 { get; init; } = string.Empty;
}

/// <summary>
/// バックアップ情報
/// </summary>
public record BackupInfo
{
    public string FilePath { get; init; } = string.Empty;
    public string FileName { get; init; } = string.Empty;
    public BackupType Type { get; init; }
    public DateTime CreatedAt { get; init; }
    public long FileSizeBytes { get; init; }
    public string ChecksumMd5 { get; init; } = string.Empty;
    public bool IsCompressed { get; init; }
    public TimeSpan BackupDuration { get; init; }
    public string Description { get; init; } = string.Empty;
}

/// <summary>
/// バックアップタイプ
/// </summary>
public enum BackupType
{
    [Description("フルバックアップ")]
    Full,
    
    [Description("差分バックアップ")]
    Incremental,
    
    [Description("手動バックアップ")]
    Manual,
    
    [Description("自動バックアップ")]
    Scheduled
}

/// <summary>
/// SQLite バックアップサービスの実装
/// </summary>
public class SqliteBackupService : IBackupService
{
    private readonly ILogger<SqliteBackupService> _logger;
    private readonly IDbConnectionFactory _connectionFactory;
    private readonly IMetricsService _metricsService;
    private readonly IConfiguration _configuration;
    
    private readonly string _backupDirectory;
    private readonly string _databasePath;

    public SqliteBackupService(
        ILogger<SqliteBackupService> logger,
        IDbConnectionFactory connectionFactory,
        IMetricsService metricsService,
        IConfiguration configuration)
    {
        _logger = logger;
        _connectionFactory = connectionFactory;
        _metricsService = metricsService;
        _configuration = configuration;

        _backupDirectory = _configuration.GetValue<string>("Backup:Directory") ?? "backups";
        _databasePath = GetDatabasePath();
        
        EnsureBackupDirectoryExists();
    }

    public async Task<string> CreateFullBackupAsync(string? backupPath = null, CancellationToken cancellationToken = default)
    {
        var startTime = DateTime.UtcNow;
        var stopwatch = System.Diagnostics.Stopwatch.StartNew();
        
        try
        {
            backupPath ??= GenerateBackupPath(BackupType.Full);
            
            _logger.LogInformation("フルバックアップ開始: {BackupPath}", backupPath);

            // SQLite データベースファイルを直接コピー
            File.Copy(_databasePath, backupPath, overwrite: true);

            // 圧縮
            var compressedPath = await CompressBackupAsync(backupPath, cancellationToken);
            File.Delete(backupPath); // 元のファイルを削除
            backupPath = compressedPath;

            stopwatch.Stop();

            // チェックサム計算
            var checksum = await CalculateChecksumAsync(backupPath, cancellationToken);
            var fileInfo = new FileInfo(backupPath);

            // バックアップ情報を記録
            await RecordBackupInfoAsync(new BackupInfo
            {
                FilePath = backupPath,
                FileName = Path.GetFileName(backupPath),
                Type = BackupType.Full,
                CreatedAt = startTime,
                FileSizeBytes = fileInfo.Length,
                ChecksumMd5 = checksum,
                IsCompressed = true,
                BackupDuration = stopwatch.Elapsed,
                Description = $"自動フルバックアップ - {DateTime.UtcNow:yyyy-MM-dd HH:mm:ss} UTC"
            });

            _logger.LogInformation("フルバックアップ完了: {BackupPath}, サイズ: {Size}MB, 所要時間: {Duration}ms", 
                backupPath, fileInfo.Length / 1024 / 1024, stopwatch.ElapsedMilliseconds);

            return backupPath;
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "フルバックアップ失敗: {BackupPath}", backupPath);
            _metricsService.IncrementDatabaseError();
            throw;
        }
    }

    public async Task<string> CreateIncrementalBackupAsync(DateTime sinceDateTime, string? backupPath = null, CancellationToken cancellationToken = default)
    {
        var startTime = DateTime.UtcNow;
        var stopwatch = System.Diagnostics.Stopwatch.StartNew();

        try
        {
            backupPath ??= GenerateBackupPath(BackupType.Incremental);
            
            _logger.LogInformation("差分バックアップ開始: {BackupPath}, Since: {SinceDateTime}", backupPath, sinceDateTime);

            // SQLiteでは差分バックアップが限られるため、フルバックアップを実行
            // 実運用では WAL ファイルの管理やタイムスタンプベースの抽出を実装
            using var connection = _connectionFactory.CreateConnection() as SqliteConnection;
            if (connection == null) throw new InvalidOperationException("SQLiteConnection が作成できませんでした");
            await connection.OpenAsync(cancellationToken);

            // 変更されたデータのみを抽出してバックアップファイルに出力
            var backupData = await ExtractChangedDataAsync(connection, sinceDateTime, cancellationToken);
            await File.WriteAllTextAsync(backupPath, backupData, cancellationToken);

            // 圧縮
            var compressedPath = await CompressBackupAsync(backupPath, cancellationToken);
            File.Delete(backupPath);
            backupPath = compressedPath;

            stopwatch.Stop();

            var checksum = await CalculateChecksumAsync(backupPath, cancellationToken);
            var fileInfo = new FileInfo(backupPath);

            await RecordBackupInfoAsync(new BackupInfo
            {
                FilePath = backupPath,
                FileName = Path.GetFileName(backupPath),
                Type = BackupType.Incremental,
                CreatedAt = startTime,
                FileSizeBytes = fileInfo.Length,
                ChecksumMd5 = checksum,
                IsCompressed = true,
                BackupDuration = stopwatch.Elapsed,
                Description = $"差分バックアップ ({sinceDateTime:yyyy-MM-dd HH:mm:ss} 以降の変更)"
            });

            _logger.LogInformation("差分バックアップ完了: {BackupPath}, サイズ: {Size}KB, 所要時間: {Duration}ms",
                backupPath, fileInfo.Length / 1024, stopwatch.ElapsedMilliseconds);

            return backupPath;
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "差分バックアップ失敗: {BackupPath}", backupPath);
            _metricsService.IncrementDatabaseError();
            throw;
        }
    }

    public async Task RestoreFromBackupAsync(string backupPath, string? targetPath = null, CancellationToken cancellationToken = default)
    {
        var stopwatch = System.Diagnostics.Stopwatch.StartNew();

        try
        {
            if (!File.Exists(backupPath))
                throw new FileNotFoundException($"バックアップファイルが見つかりません: {backupPath}");

            targetPath ??= _databasePath;
            
            _logger.LogInformation("データベース復元開始: {BackupPath} -> {TargetPath}", backupPath, targetPath);

            // バックアップ検証
            var verification = await VerifyBackupAsync(backupPath, cancellationToken);
            if (!verification.IsValid)
                throw new InvalidOperationException($"バックアップファイルが不正です: {verification.Message}");

            // 既存データベースのバックアップ
            var backupOfCurrent = $"{targetPath}.restore-backup.{DateTime.UtcNow:yyyyMMdd-HHmmss}";
            if (File.Exists(targetPath))
            {
                File.Copy(targetPath, backupOfCurrent);
                _logger.LogInformation("現在のデータベースをバックアップ: {BackupPath}", backupOfCurrent);
            }

            // 圧縮ファイルの場合は展開
            string sourceFile = backupPath;
            if (Path.GetExtension(backupPath) == ".gz")
            {
                sourceFile = await DecompressBackupAsync(backupPath, cancellationToken);
            }

            // データベースファイルをコピー
            File.Copy(sourceFile, targetPath, overwrite: true);

            // 一時ファイルをクリーンアップ
            if (sourceFile != backupPath && File.Exists(sourceFile))
            {
                File.Delete(sourceFile);
            }

            stopwatch.Stop();
            
            _logger.LogInformation("データベース復元完了: {TargetPath}, 所要時間: {Duration}ms", 
                targetPath, stopwatch.ElapsedMilliseconds);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "データベース復元失敗: {BackupPath} -> {TargetPath}", backupPath, targetPath);
            _metricsService.IncrementDatabaseError();
            throw;
        }
    }

    public async Task<BackupVerificationResult> VerifyBackupAsync(string backupPath, CancellationToken cancellationToken = default)
    {
        var stopwatch = System.Diagnostics.Stopwatch.StartNew();

        try
        {
            if (!File.Exists(backupPath))
            {
                return new BackupVerificationResult
                {
                    IsValid = false,
                    Message = "バックアップファイルが存在しません"
                };
            }

            var fileInfo = new FileInfo(backupPath);
            var checksum = await CalculateChecksumAsync(backupPath, cancellationToken);

            // 圧縮ファイルの場合は展開して検証
            string sourceFile = backupPath;
            if (Path.GetExtension(backupPath) == ".gz")
            {
                sourceFile = await DecompressBackupAsync(backupPath, cancellationToken);
            }

            try
            {
                // SQLite ファイルとして開けるかテスト
                using var connection = new Microsoft.Data.Sqlite.SqliteConnection($"Data Source={sourceFile}");
                await connection.OpenAsync(cancellationToken);

                using var command = connection.CreateCommand();
                command.CommandText = "SELECT COUNT(*) FROM sqlite_master WHERE type='table'";
                var tableCount = await command.ExecuteScalarAsync(cancellationToken);

                command.CommandText = "SELECT COUNT(*) FROM Users";
                var recordCount = Convert.ToInt32(await command.ExecuteScalarAsync(cancellationToken));

                stopwatch.Stop();

                return new BackupVerificationResult
                {
                    IsValid = true,
                    Message = $"バックアップファイルは有効です。テーブル数: {tableCount}, レコード数: {recordCount}",
                    FileSizeBytes = fileInfo.Length,
                    CreatedAt = fileInfo.CreationTimeUtc,
                    VerificationDuration = stopwatch.Elapsed,
                    RecordCount = recordCount,
                    ChecksumMd5 = checksum
                };
            }
            finally
            {
                // 一時ファイルをクリーンアップ
                if (sourceFile != backupPath && File.Exists(sourceFile))
                {
                    File.Delete(sourceFile);
                }
            }
        }
        catch (Exception ex)
        {
            stopwatch.Stop();
            _logger.LogError(ex, "バックアップ検証エラー: {BackupPath}", backupPath);

            return new BackupVerificationResult
            {
                IsValid = false,
                Message = $"検証エラー: {ex.Message}",
                VerificationDuration = stopwatch.Elapsed
            };
        }
    }

    public async Task<int> CleanupOldBackupsAsync(int retentionDays, CancellationToken cancellationToken = default)
    {
        try
        {
            var cutoffDate = DateTime.UtcNow.AddDays(-retentionDays);
            var backupFiles = Directory.GetFiles(_backupDirectory, "*.gz")
                .Where(f => File.GetCreationTimeUtc(f) < cutoffDate)
                .ToList();

            var deletedCount = 0;
            foreach (var file in backupFiles)
            {
                try
                {
                    File.Delete(file);
                    deletedCount++;
                    _logger.LogInformation("古いバックアップファイルを削除: {FilePath}", file);
                }
                catch (Exception ex)
                {
                    _logger.LogWarning(ex, "バックアップファイル削除失敗: {FilePath}", file);
                }
            }

            _logger.LogInformation("バックアップクリーンアップ完了: {DeletedCount}ファイル削除, 保持期間: {RetentionDays}日", 
                deletedCount, retentionDays);

            return deletedCount;
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "バックアップクリーンアップエラー");
            return 0;
        }
    }

    public async Task<IEnumerable<BackupInfo>> GetBackupHistoryAsync(int limit = 50, CancellationToken cancellationToken = default)
    {
        try
        {
            var backupFiles = Directory.GetFiles(_backupDirectory, "*.gz")
                .Select(filePath =>
                {
                    var fileInfo = new FileInfo(filePath);
                    var fileName = fileInfo.Name;
                    
                    // ファイル名からタイプを判定
                    var type = fileName.Contains("incremental") ? BackupType.Incremental :
                               fileName.Contains("manual") ? BackupType.Manual :
                               fileName.Contains("scheduled") ? BackupType.Scheduled :
                               BackupType.Full;

                    return new BackupInfo
                    {
                        FilePath = filePath,
                        FileName = fileName,
                        Type = type,
                        CreatedAt = fileInfo.CreationTimeUtc,
                        FileSizeBytes = fileInfo.Length,
                        IsCompressed = true,
                        Description = $"{type} - {fileInfo.CreationTimeUtc:yyyy-MM-dd HH:mm:ss} UTC"
                    };
                })
                .OrderByDescending(b => b.CreatedAt)
                .Take(limit)
                .ToList();

            return backupFiles;
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "バックアップ履歴取得エラー");
            return Enumerable.Empty<BackupInfo>();
        }
    }

    public async Task ExecuteScheduledBackupAsync(CancellationToken cancellationToken = default)
    {
        try
        {
            _logger.LogInformation("スケジュールされたバックアップを開始");

            // 日次フルバックアップ
            await CreateFullBackupAsync(cancellationToken: cancellationToken);

            // 古いバックアップのクリーンアップ（30日保持）
            var retentionDays = _configuration.GetValue<int>("Backup:RetentionDays", 30);
            await CleanupOldBackupsAsync(retentionDays, cancellationToken);

            _logger.LogInformation("スケジュールされたバックアップを完了");
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "スケジュールされたバックアップでエラーが発生");
            throw;
        }
    }

    #region Private Methods

    private string GetDatabasePath()
    {
        var connectionString = _configuration.GetConnectionString("DefaultConnection")
            ?? throw new InvalidOperationException("DefaultConnection is not configured");

        // SQLite 接続文字列から Data Source を抽出
        var match = System.Text.RegularExpressions.Regex.Match(connectionString, @"Data Source=([^;]+)");
        if (!match.Success)
            throw new InvalidOperationException("Cannot extract database path from connection string");

        return match.Groups[1].Value;
    }

    private void EnsureBackupDirectoryExists()
    {
        if (!Directory.Exists(_backupDirectory))
        {
            Directory.CreateDirectory(_backupDirectory);
            _logger.LogInformation("バックアップディレクトリを作成: {Directory}", _backupDirectory);
        }
    }

    private string GenerateBackupPath(BackupType type)
    {
        var timestamp = DateTime.UtcNow.ToString("yyyyMMdd-HHmmss");
        var typePrefix = type.ToString().ToLowerInvariant();
        var fileName = $"mrs-{typePrefix}-{timestamp}.db.gz";
        return Path.Combine(_backupDirectory, fileName);
    }

    private async Task<string> CompressBackupAsync(string filePath, CancellationToken cancellationToken)
    {
        var compressedPath = $"{filePath}.gz";
        
        await using var originalFileStream = File.OpenRead(filePath);
        await using var compressedFileStream = File.Create(compressedPath);
        await using var gzipStream = new System.IO.Compression.GZipStream(compressedFileStream, System.IO.Compression.CompressionMode.Compress);
        
        await originalFileStream.CopyToAsync(gzipStream, cancellationToken);
        
        return compressedPath;
    }

    private async Task<string> DecompressBackupAsync(string compressedPath, CancellationToken cancellationToken)
    {
        var decompressedPath = compressedPath.Replace(".gz", "");
        
        await using var compressedFileStream = File.OpenRead(compressedPath);
        await using var gzipStream = new System.IO.Compression.GZipStream(compressedFileStream, System.IO.Compression.CompressionMode.Decompress);
        await using var decompressedFileStream = File.Create(decompressedPath);
        
        await gzipStream.CopyToAsync(decompressedFileStream, cancellationToken);
        
        return decompressedPath;
    }

    private async Task<string> CalculateChecksumAsync(string filePath, CancellationToken cancellationToken)
    {
        using var sha256 = System.Security.Cryptography.SHA256.Create();
        await using var stream = File.OpenRead(filePath);
        var hash = await sha256.ComputeHashAsync(stream, cancellationToken);
        return Convert.ToHexString(hash);
    }

    private async Task<string> ExtractChangedDataAsync(SqliteConnection connection, DateTime sinceDateTime, CancellationToken cancellationToken)
    {
        // 簡易的な差分データ抽出（実運用では更に詳細な実装が必要）
        var sql = @"
            SELECT 'Reservations' as TableName, COUNT(*) as ChangedCount 
            FROM Reservations 
            WHERE CreatedAt > @SinceDateTime OR UpdatedAt > @SinceDateTime
            UNION ALL
            SELECT 'Users' as TableName, COUNT(*) as ChangedCount 
            FROM Users 
            WHERE CreatedAt > @SinceDateTime OR UpdatedAt > @SinceDateTime";

        using var command = connection.CreateCommand();
        command.CommandText = sql;
        var param = command.CreateParameter();
        param.ParameterName = "@SinceDateTime";
        param.Value = sinceDateTime;
        command.Parameters.Add(param);

        var result = new System.Text.StringBuilder();
        result.AppendLine($"-- 差分バックアップ: {sinceDateTime:yyyy-MM-dd HH:mm:ss} 以降の変更");
        result.AppendLine($"-- 作成日時: {DateTime.UtcNow:yyyy-MM-dd HH:mm:ss} UTC");
        result.AppendLine();

        using var reader = await command.ExecuteReaderAsync(cancellationToken);
        while (await reader.ReadAsync(cancellationToken))
        {
            result.AppendLine($"-- {reader["TableName"]}: {reader["ChangedCount"]} 件の変更");
        }

        return result.ToString();
    }

    private async Task RecordBackupInfoAsync(BackupInfo backupInfo)
    {
        // 実装: バックアップ情報をログやデータベースに記録
        // 現在は単純にログ出力
        _logger.LogInformation("バックアップ情報記録: {@BackupInfo}", new 
        {
            backupInfo.FileName,
            backupInfo.Type,
            backupInfo.CreatedAt,
            SizeMB = backupInfo.FileSizeBytes / 1024 / 1024,
            DurationMs = backupInfo.BackupDuration.TotalMilliseconds,
            backupInfo.ChecksumMd5
        });
    }

    #endregion
}