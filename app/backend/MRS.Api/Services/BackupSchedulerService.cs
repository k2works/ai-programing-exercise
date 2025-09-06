namespace MRS.Api.Services;

/// <summary>
/// バックアップスケジューラーのバックグラウンドサービス
/// </summary>
public class BackupSchedulerService : BackgroundService
{
    private readonly ILogger<BackupSchedulerService> _logger;
    private readonly IServiceScopeFactory _serviceScopeFactory;
    private readonly IConfiguration _configuration;

    public BackupSchedulerService(
        ILogger<BackupSchedulerService> logger,
        IServiceScopeFactory serviceScopeFactory,
        IConfiguration configuration)
    {
        _logger = logger;
        _serviceScopeFactory = serviceScopeFactory;
        _configuration = configuration;
    }

    protected override async Task ExecuteAsync(CancellationToken stoppingToken)
    {
        _logger.LogInformation("バックアップスケジューラーサービス開始");

        // 初回実行まで待機（アプリケーション完全起動まで）
        await Task.Delay(TimeSpan.FromMinutes(1), stoppingToken);

        while (!stoppingToken.IsCancellationRequested)
        {
            try
            {
                await ExecuteScheduledTasksAsync(stoppingToken);
                
                // 次回実行まで待機（1時間間隔でチェック）
                await Task.Delay(TimeSpan.FromHours(1), stoppingToken);
            }
            catch (OperationCanceledException)
            {
                _logger.LogInformation("バックアップスケジューラーサービス停止要求");
                break;
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "バックアップスケジューラーでエラーが発生");
                
                // エラー発生時は短時間待機してリトライ
                await Task.Delay(TimeSpan.FromMinutes(5), stoppingToken);
            }
        }

        _logger.LogInformation("バックアップスケジューラーサービス終了");
    }

    private async Task ExecuteScheduledTasksAsync(CancellationToken cancellationToken)
    {
        var now = DateTime.UtcNow;
        var currentHour = now.Hour;

        // 設定からバックアップ時刻を取得（デフォルト: 02:00 UTC）
        var dailyBackupHour = _configuration.GetValue<int>("Backup:DailyBackupHour", 2);
        var incrementalBackupInterval = _configuration.GetValue<int>("Backup:IncrementalBackupIntervalHours", 4);

        using var scope = _serviceScopeFactory.CreateScope();
        var backupService = scope.ServiceProvider.GetRequiredService<IBackupService>();

        try
        {
            // 日次フルバックアップ（指定時刻に実行）
            if (ShouldExecuteDailyBackup(currentHour, dailyBackupHour))
            {
                _logger.LogInformation("日次フルバックアップ実行開始: {Time}", now);
                await backupService.ExecuteScheduledBackupAsync(cancellationToken);
            }

            // 増分バックアップ（4時間間隔）
            if (ShouldExecuteIncrementalBackup(currentHour, incrementalBackupInterval))
            {
                _logger.LogInformation("増分バックアップ実行開始: {Time}", now);
                var sinceDateTime = now.AddHours(-incrementalBackupInterval);
                await backupService.CreateIncrementalBackupAsync(sinceDateTime, cancellationToken: cancellationToken);
            }

            // バックアップファイル検証（日次）
            if (ShouldExecuteVerification(currentHour, dailyBackupHour))
            {
                await ExecuteBackupVerificationAsync(backupService, cancellationToken);
            }
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "スケジュールされたバックアップ実行中にエラーが発生");
            throw;
        }
    }

    private bool ShouldExecuteDailyBackup(int currentHour, int targetHour)
    {
        // 指定された時間帯でのみ実行（例：02:00-02:59）
        return currentHour == targetHour;
    }

    private bool ShouldExecuteIncrementalBackup(int currentHour, int intervalHours)
    {
        // 設定されたインターバルで実行（例：4時間間隔 -> 0, 4, 8, 12, 16, 20時）
        return currentHour % intervalHours == 0;
    }

    private bool ShouldExecuteVerification(int currentHour, int targetHour)
    {
        // 日次バックアップの1時間後に検証実行
        return currentHour == (targetHour + 1) % 24;
    }

    private async Task ExecuteBackupVerificationAsync(IBackupService backupService, CancellationToken cancellationToken)
    {
        try
        {
            _logger.LogInformation("バックアップファイル検証開始");

            // 最新のバックアップファイルを検証
            var backupHistory = await backupService.GetBackupHistoryAsync(5, cancellationToken);
            var latestBackups = backupHistory.Take(3).ToList();

            foreach (var backup in latestBackups)
            {
                var result = await backupService.VerifyBackupAsync(backup.FilePath, cancellationToken);
                
                if (result.IsValid)
                {
                    _logger.LogInformation("バックアップファイル検証成功: {FilePath} - {Message}", 
                        backup.FileName, result.Message);
                }
                else
                {
                    _logger.LogError("バックアップファイル検証失敗: {FilePath} - {Message}", 
                        backup.FileName, result.Message);
                }
            }

            _logger.LogInformation("バックアップファイル検証完了");
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "バックアップファイル検証中にエラーが発生");
        }
    }
}

/// <summary>
/// 災害復旧テストサービス
/// </summary>
public class DisasterRecoveryTestService : BackgroundService
{
    private readonly ILogger<DisasterRecoveryTestService> _logger;
    private readonly IServiceScopeFactory _serviceScopeFactory;
    private readonly IConfiguration _configuration;

    public DisasterRecoveryTestService(
        ILogger<DisasterRecoveryTestService> logger,
        IServiceScopeFactory serviceScopeFactory,
        IConfiguration configuration)
    {
        _logger = logger;
        _serviceScopeFactory = serviceScopeFactory;
        _configuration = configuration;
    }

    protected override async Task ExecuteAsync(CancellationToken stoppingToken)
    {
        _logger.LogInformation("災害復旧テストサービス開始");

        // 初回実行まで長時間待機（1日後）
        await Task.Delay(TimeSpan.FromDays(1), stoppingToken);

        while (!stoppingToken.IsCancellationRequested)
        {
            try
            {
                // 週次災害復旧テスト実行
                await ExecuteWeeklyRecoveryTestAsync(stoppingToken);
                
                // 次回実行まで待機（7日間）
                await Task.Delay(TimeSpan.FromDays(7), stoppingToken);
            }
            catch (OperationCanceledException)
            {
                _logger.LogInformation("災害復旧テストサービス停止要求");
                break;
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "災害復旧テストでエラーが発生");
                
                // エラー発生時は1日後にリトライ
                await Task.Delay(TimeSpan.FromDays(1), stoppingToken);
            }
        }

        _logger.LogInformation("災害復旧テストサービス終了");
    }

    private async Task ExecuteWeeklyRecoveryTestAsync(CancellationToken cancellationToken)
    {
        var startTime = DateTime.UtcNow;
        _logger.LogInformation("週次災害復旧テスト開始: {StartTime}", startTime);

        using var scope = _serviceScopeFactory.CreateScope();
        var backupService = scope.ServiceProvider.GetRequiredService<IBackupService>();

        try
        {
            // 1. 最新のバックアップファイルを特定
            var backupHistory = await backupService.GetBackupHistoryAsync(10, cancellationToken);
            var latestFullBackup = backupHistory.FirstOrDefault(b => b.Type == BackupType.Full);
            
            if (latestFullBackup == null)
            {
                _logger.LogWarning("災害復旧テスト用のフルバックアップが見つかりません");
                return;
            }

            // 2. バックアップファイル検証
            _logger.LogInformation("災害復旧テスト - バックアップ検証: {BackupFile}", latestFullBackup.FileName);
            var verification = await backupService.VerifyBackupAsync(latestFullBackup.FilePath, cancellationToken);
            
            if (!verification.IsValid)
            {
                _logger.LogError("災害復旧テスト失敗 - バックアップファイル不正: {Message}", verification.Message);
                return;
            }

            // 3. テスト用データベースへの復元
            var testDatabasePath = $"test-recovery-{DateTime.UtcNow:yyyyMMdd-HHmmss}.db";
            _logger.LogInformation("災害復旧テスト - テストDB復元: {TestDbPath}", testDatabasePath);
            
            await backupService.RestoreFromBackupAsync(latestFullBackup.FilePath, testDatabasePath, cancellationToken);

            // 4. 復元データベースの整合性テスト
            await ValidateRestoredDatabaseAsync(testDatabasePath, cancellationToken);

            // 5. 復旧時間測定
            var recoveryTime = DateTime.UtcNow - startTime;
            var targetRecoveryTime = TimeSpan.FromHours(4); // RTO: 4時間

            if (recoveryTime <= targetRecoveryTime)
            {
                _logger.LogInformation("災害復旧テスト成功: 復旧時間 {RecoveryTime} (目標: {TargetTime})", 
                    recoveryTime, targetRecoveryTime);
            }
            else
            {
                _logger.LogWarning("災害復旧テスト警告: 復旧時間が目標を超過 {RecoveryTime} > {TargetTime}", 
                    recoveryTime, targetRecoveryTime);
            }

            // 6. テストファイルクリーンアップ
            if (File.Exists(testDatabasePath))
            {
                File.Delete(testDatabasePath);
                _logger.LogInformation("災害復旧テスト - テストDB削除: {TestDbPath}", testDatabasePath);
            }

            _logger.LogInformation("週次災害復旧テスト完了: 総所要時間 {TotalTime}", DateTime.UtcNow - startTime);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "週次災害復旧テスト中にエラーが発生");
            throw;
        }
    }

    private async Task ValidateRestoredDatabaseAsync(string databasePath, CancellationToken cancellationToken)
    {
        try
        {
            using var connection = new Microsoft.Data.Sqlite.SqliteConnection($"Data Source={databasePath}");
            await connection.OpenAsync(cancellationToken);

            // テーブル存在チェック
            using var command = connection.CreateCommand();
            command.CommandText = "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name";
            
            var tables = new List<string>();
            using var reader = await command.ExecuteReaderAsync(cancellationToken);
            while (await reader.ReadAsync(cancellationToken))
            {
                tables.Add(reader.GetString(0));
            }

            var expectedTables = new[] { "Users", "Rooms", "Reservations" };
            var missingTables = expectedTables.Except(tables).ToList();
            
            if (missingTables.Count > 0)
            {
                throw new InvalidOperationException($"復元データベースに必要なテーブルがありません: {string.Join(", ", missingTables)}");
            }

            // データ整合性チェック
            command.CommandText = @"
                SELECT 
                    (SELECT COUNT(*) FROM Users) as UserCount,
                    (SELECT COUNT(*) FROM Rooms) as RoomCount,
                    (SELECT COUNT(*) FROM Reservations) as ReservationCount";

            using var dataReader = await command.ExecuteReaderAsync(cancellationToken);
            if (await dataReader.ReadAsync(cancellationToken))
            {
                var userCount = dataReader.GetInt32(0); // UserCount
                var roomCount = dataReader.GetInt32(1); // RoomCount
                var reservationCount = dataReader.GetInt32(2); // ReservationCount

                _logger.LogInformation("復元データベース検証 - Users: {UserCount}, Rooms: {RoomCount}, Reservations: {ReservationCount}",
                    userCount, roomCount, reservationCount);

                if (userCount == 0 && roomCount == 0)
                {
                    _logger.LogWarning("復元データベースにマスターデータが存在しません");
                }
            }

            _logger.LogInformation("復元データベースの整合性検証成功: {DatabasePath}", databasePath);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "復元データベースの整合性検証失敗: {DatabasePath}", databasePath);
            throw;
        }
    }
}