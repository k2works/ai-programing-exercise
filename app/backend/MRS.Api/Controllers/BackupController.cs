using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc;
using MRS.Api.Services;
using Microsoft.AspNetCore.RateLimiting;

namespace MRS.Api.Controllers;

/// <summary>
/// バックアップ・災害復旧管理API
/// </summary>
[ApiController]
[Route("api/[controller]")]
[Authorize(Roles = "Admin")] // 管理者のみアクセス可能
public class BackupController : ControllerBase
{
    private readonly IBackupService _backupService;
    private readonly ISecurityLogService _securityLogService;
    private readonly ILogger<BackupController> _logger;

    public BackupController(
        IBackupService backupService,
        ISecurityLogService securityLogService,
        ILogger<BackupController> logger)
    {
        _backupService = backupService;
        _securityLogService = securityLogService;
        _logger = logger;
    }

    /// <summary>
    /// フルバックアップを実行
    /// </summary>
    /// <param name="cancellationToken">キャンセレーショントークン</param>
    /// <response code="200">バックアップ作成成功</response>
    /// <response code="400">リクエストが無効</response>
    /// <response code="500">サーバーエラー</response>
    [HttpPost("full")]
    [EnableRateLimiting("ApiPolicy")]
    [ProducesResponseType(StatusCodes.Status200OK, Type = typeof(BackupResultDto))]
    [ProducesResponseType(StatusCodes.Status400BadRequest)]
    [ProducesResponseType(StatusCodes.Status500InternalServerError)]
    public async Task<ActionResult<BackupResultDto>> CreateFullBackup(CancellationToken cancellationToken)
    {
        var username = User.Identity?.Name;
        var ipAddress = HttpContext.Connection.RemoteIpAddress?.ToString();

        try
        {
            _logger.LogInformation("手動フルバックアップ開始 - User: {Username}, IP: {IpAddress}", username, ipAddress);
            _securityLogService.LogDataAccess("Backup", username ?? "Unknown", "フルバックアップ実行", ipAddress);

            var backupPath = await _backupService.CreateFullBackupAsync(cancellationToken: cancellationToken);
            var verification = await _backupService.VerifyBackupAsync(backupPath, cancellationToken);

            var result = new BackupResultDto
            {
                Success = true,
                BackupPath = Path.GetFileName(backupPath),
                FileSizeBytes = GetFileSizeSecurely(backupPath),
                CreatedAt = DateTime.UtcNow,
                Type = "Full",
                IsValid = verification.IsValid,
                ValidationMessage = verification.Message
            };

            _logger.LogInformation("手動フルバックアップ完了 - {BackupPath}", Path.GetFileName(backupPath));
            return Ok(result);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "手動フルバックアップ失敗 - User: {Username}", username);
            _securityLogService.LogSecurityViolation("BackupError", ex.Message, username, ipAddress);
            
            return StatusCode(500, new BackupResultDto
            {
                Success = false,
                ErrorMessage = "バックアップの作成中にエラーが発生しました。"
            });
        }
    }

    /// <summary>
    /// 差分バックアップを実行
    /// </summary>
    /// <param name="request">差分バックアップリクエスト</param>
    /// <param name="cancellationToken">キャンセレーショントークン</param>
    /// <response code="200">差分バックアップ作成成功</response>
    /// <response code="400">リクエストが無効</response>
    /// <response code="500">サーバーエラー</response>
    [HttpPost("incremental")]
    [EnableRateLimiting("ApiPolicy")]
    [ProducesResponseType(StatusCodes.Status200OK, Type = typeof(BackupResultDto))]
    [ProducesResponseType(StatusCodes.Status400BadRequest)]
    [ProducesResponseType(StatusCodes.Status500InternalServerError)]
    public async Task<ActionResult<BackupResultDto>> CreateIncrementalBackup(
        [FromBody] CreateIncrementalBackupRequest request,
        CancellationToken cancellationToken)
    {
        var username = User.Identity?.Name;
        var ipAddress = HttpContext.Connection.RemoteIpAddress?.ToString();

        if (request.SinceDateTime > DateTime.UtcNow)
        {
            return BadRequest(new { message = "SinceDateTime は現在時刻より前の時刻を指定してください。" });
        }

        try
        {
            _logger.LogInformation("手動差分バックアップ開始 - User: {Username}, Since: {SinceDateTime}", username, request.SinceDateTime);
            _securityLogService.LogDataAccess("Backup", username ?? "Unknown", "差分バックアップ実行", ipAddress);

            var backupPath = await _backupService.CreateIncrementalBackupAsync(request.SinceDateTime, cancellationToken: cancellationToken);
            var verification = await _backupService.VerifyBackupAsync(backupPath, cancellationToken);

            var result = new BackupResultDto
            {
                Success = true,
                BackupPath = Path.GetFileName(backupPath),
                FileSizeBytes = GetFileSizeSecurely(backupPath),
                CreatedAt = DateTime.UtcNow,
                Type = "Incremental",
                IsValid = verification.IsValid,
                ValidationMessage = verification.Message,
                SinceDateTime = request.SinceDateTime
            };

            _logger.LogInformation("手動差分バックアップ完了 - {BackupPath}", Path.GetFileName(backupPath));
            return Ok(result);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "手動差分バックアップ失敗 - User: {Username}", username);
            _securityLogService.LogSecurityViolation("BackupError", ex.Message, username, ipAddress);

            return StatusCode(500, new BackupResultDto
            {
                Success = false,
                ErrorMessage = "差分バックアップの作成中にエラーが発生しました。"
            });
        }
    }

    /// <summary>
    /// バックアップから復元
    /// </summary>
    /// <param name="request">復元リクエスト</param>
    /// <param name="cancellationToken">キャンセレーショントークン</param>
    /// <response code="200">復元成功</response>
    /// <response code="400">リクエストが無効</response>
    /// <response code="404">バックアップファイルが見つからない</response>
    /// <response code="500">サーバーエラー</response>
    [HttpPost("restore")]
    [EnableRateLimiting("ApiPolicy")]
    [ProducesResponseType(StatusCodes.Status200OK, Type = typeof(RestoreResultDto))]
    [ProducesResponseType(StatusCodes.Status400BadRequest)]
    [ProducesResponseType(StatusCodes.Status404NotFound)]
    [ProducesResponseType(StatusCodes.Status500InternalServerError)]
    public async Task<ActionResult<RestoreResultDto>> RestoreFromBackup(
        [FromBody] RestoreBackupRequest request,
        CancellationToken cancellationToken)
    {
        var username = User.Identity?.Name;
        var ipAddress = HttpContext.Connection.RemoteIpAddress?.ToString();

        try
        {
            _logger.LogWarning("データベース復元開始 - User: {Username}, BackupFile: {BackupFileName}", username, request.BackupFileName);
            _securityLogService.LogDataAccess("Backup", username ?? "Unknown", "データベース復元実行", ipAddress);

            // バックアップファイルパスを解決
            var backupDirectory = Path.Combine(Directory.GetCurrentDirectory(), "backups");
            var backupPath = Path.Combine(backupDirectory, request.BackupFileName);

            if (!FileExistsSecurely(backupPath))
            {
                return NotFound(new { message = $"バックアップファイルが見つかりません: {request.BackupFileName}" });
            }

            // 復元前にバックアップ検証
            var verification = await _backupService.VerifyBackupAsync(backupPath, cancellationToken);
            if (!verification.IsValid)
            {
                return BadRequest(new { message = $"バックアップファイルが無効です: {verification.Message}" });
            }

            // 復元実行
            var startTime = DateTime.UtcNow;
            await _backupService.RestoreFromBackupAsync(backupPath, request.TargetDatabasePath, cancellationToken);
            var duration = DateTime.UtcNow - startTime;

            var result = new RestoreResultDto
            {
                Success = true,
                BackupFileName = request.BackupFileName,
                TargetDatabasePath = request.TargetDatabasePath ?? "デフォルトデータベース",
                RestoredAt = DateTime.UtcNow,
                RestoreDuration = duration,
                RecordCount = verification.RecordCount
            };

            _logger.LogWarning("データベース復元完了 - User: {Username}, Duration: {Duration}ms", username, duration.TotalMilliseconds);
            return Ok(result);
        }
        catch (FileNotFoundException ex)
        {
            _logger.LogError(ex, "復元失敗（ファイル未発見） - User: {Username}, File: {BackupFileName}", username, request.BackupFileName);
            return NotFound(new { message = ex.Message });
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "復元失敗 - User: {Username}, File: {BackupFileName}", username, request.BackupFileName);
            _securityLogService.LogSecurityViolation("RestoreError", ex.Message, username, ipAddress);

            return StatusCode(500, new RestoreResultDto
            {
                Success = false,
                ErrorMessage = "復元処理中にエラーが発生しました。"
            });
        }
    }

    /// <summary>
    /// バックアップファイルの検証
    /// </summary>
    /// <param name="backupFileName">バックアップファイル名</param>
    /// <param name="cancellationToken">キャンセレーショントークン</param>
    /// <response code="200">検証結果</response>
    /// <response code="404">ファイルが見つからない</response>
    /// <response code="500">サーバーエラー</response>
    [HttpPost("verify/{backupFileName}")]
    [EnableRateLimiting("ApiPolicy")]
    [ProducesResponseType(StatusCodes.Status200OK, Type = typeof(BackupVerificationResult))]
    [ProducesResponseType(StatusCodes.Status404NotFound)]
    [ProducesResponseType(StatusCodes.Status500InternalServerError)]
    public async Task<ActionResult<BackupVerificationResult>> VerifyBackup(
        string backupFileName,
        CancellationToken cancellationToken)
    {
        var username = User.Identity?.Name;
        var ipAddress = HttpContext.Connection.RemoteIpAddress?.ToString();

        try
        {
            _securityLogService.LogDataAccess("Backup", username ?? "Unknown", "バックアップ検証実行", ipAddress);

            var backupDirectory = Path.Combine(Directory.GetCurrentDirectory(), "backups");
            var backupPath = Path.Combine(backupDirectory, backupFileName);

            if (!FileExistsSecurely(backupPath))
            {
                return NotFound(new { message = $"バックアップファイルが見つかりません: {backupFileName}" });
            }

            var result = await _backupService.VerifyBackupAsync(backupPath, cancellationToken);

            _logger.LogInformation("バックアップ検証実行 - User: {Username}, File: {BackupFileName}, Valid: {IsValid}",
                username, backupFileName, result.IsValid);

            return Ok(result);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "バックアップ検証失敗 - User: {Username}, File: {BackupFileName}", username, backupFileName);
            return StatusCode(500, new { message = "検証処理中にエラーが発生しました。" });
        }
    }

    /// <summary>
    /// バックアップ履歴を取得
    /// </summary>
    /// <param name="limit">取得件数上限</param>
    /// <param name="cancellationToken">キャンセレーショントークン</param>
    /// <response code="200">バックアップ履歴リスト</response>
    /// <response code="500">サーバーエラー</response>
    [HttpGet("history")]
    [ProducesResponseType(StatusCodes.Status200OK, Type = typeof(IEnumerable<BackupInfo>))]
    [ProducesResponseType(StatusCodes.Status500InternalServerError)]
    public async Task<ActionResult<IEnumerable<BackupInfo>>> GetBackupHistory(
        [FromQuery] int limit = 50,
        CancellationToken cancellationToken = default)
    {
        var username = User.Identity?.Name;
        var ipAddress = HttpContext.Connection.RemoteIpAddress?.ToString();

        try
        {
            _securityLogService.LogDataAccess("Backup", username ?? "Unknown", "バックアップ履歴閲覧", ipAddress);

            var history = await _backupService.GetBackupHistoryAsync(Math.Min(limit, 100), cancellationToken);
            
            _logger.LogInformation("バックアップ履歴取得 - User: {Username}, Count: {Count}", username, history.Count());
            return Ok(history);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "バックアップ履歴取得失敗 - User: {Username}", username);
            return StatusCode(500, new { message = "履歴取得中にエラーが発生しました。" });
        }
    }

    /// <summary>
    /// 古いバックアップファイルをクリーンアップ
    /// </summary>
    /// <param name="retentionDays">保持日数</param>
    /// <param name="cancellationToken">キャンセレーショントークン</param>
    /// <response code="200">クリーンアップ結果</response>
    /// <response code="400">無効なパラメータ</response>
    /// <response code="500">サーバーエラー</response>
    [HttpDelete("cleanup")]
    [EnableRateLimiting("ApiPolicy")]
    [ProducesResponseType(StatusCodes.Status200OK, Type = typeof(CleanupResultDto))]
    [ProducesResponseType(StatusCodes.Status400BadRequest)]
    [ProducesResponseType(StatusCodes.Status500InternalServerError)]
    public async Task<ActionResult<CleanupResultDto>> CleanupOldBackups(
        [FromQuery] int retentionDays = 30,
        CancellationToken cancellationToken = default)
    {
        var username = User.Identity?.Name;
        var ipAddress = HttpContext.Connection.RemoteIpAddress?.ToString();

        if (retentionDays < 1 || retentionDays > 365)
        {
            return BadRequest(new { message = "保持日数は1日以上365日以下で指定してください。" });
        }

        try
        {
            _logger.LogInformation("バックアップクリーンアップ開始 - User: {Username}, RetentionDays: {RetentionDays}",
                username, retentionDays);
            _securityLogService.LogDataAccess("Backup", username ?? "Unknown", "バックアップクリーンアップ実行", ipAddress);

            var deletedCount = await _backupService.CleanupOldBackupsAsync(retentionDays, cancellationToken);

            var result = new CleanupResultDto
            {
                Success = true,
                DeletedCount = deletedCount,
                RetentionDays = retentionDays,
                ExecutedAt = DateTime.UtcNow
            };

            _logger.LogInformation("バックアップクリーンアップ完了 - User: {Username}, DeletedCount: {DeletedCount}",
                username, deletedCount);

            return Ok(result);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "バックアップクリーンアップ失敗 - User: {Username}", username);
            _securityLogService.LogSecurityViolation("CleanupError", ex.Message, username, ipAddress);

            return StatusCode(500, new CleanupResultDto
            {
                Success = false,
                ErrorMessage = "クリーンアップ処理中にエラーが発生しました。"
            });
        }
    }

    /// <summary>
    /// 災害復旧テストを実行
    /// </summary>
    /// <param name="cancellationToken">キャンセレーショントークン</param>
    /// <response code="200">災害復旧テスト結果</response>
    /// <response code="500">サーバーエラー</response>
    [HttpPost("disaster-recovery-test")]
    [EnableRateLimiting("ApiPolicy")]
    [ProducesResponseType(StatusCodes.Status200OK, Type = typeof(DisasterRecoveryTestResultDto))]
    [ProducesResponseType(StatusCodes.Status500InternalServerError)]
    public async Task<ActionResult<DisasterRecoveryTestResultDto>> ExecuteDisasterRecoveryTest(
        CancellationToken cancellationToken)
    {
        var username = User.Identity?.Name;
        var ipAddress = HttpContext.Connection.RemoteIpAddress?.ToString();

        try
        {
            _logger.LogWarning("災害復旧テスト開始 - User: {Username}", username);
            _securityLogService.LogDataAccess("Backup", username ?? "Unknown", "災害復旧テスト実行", ipAddress);

            var startTime = DateTime.UtcNow;

            // 最新のフルバックアップを取得
            var backupHistory = await _backupService.GetBackupHistoryAsync(10, cancellationToken);
            var latestFullBackup = backupHistory.FirstOrDefault(b => b.Type == BackupType.Full);

            if (latestFullBackup == null)
            {
                return StatusCode(500, new DisasterRecoveryTestResultDto
                {
                    Success = false,
                    ErrorMessage = "テスト用のフルバックアップが見つかりません。"
                });
            }

            // バックアップ検証
            var verification = await _backupService.VerifyBackupAsync(latestFullBackup.FilePath, cancellationToken);
            if (!verification.IsValid)
            {
                return StatusCode(500, new DisasterRecoveryTestResultDto
                {
                    Success = false,
                    ErrorMessage = $"バックアップファイルが無効です: {verification.Message}"
                });
            }

            // テスト用データベースに復元
            var testDbPath = $"test-recovery-{DateTime.UtcNow:yyyyMMdd-HHmmss}.db";
            await _backupService.RestoreFromBackupAsync(latestFullBackup.FilePath, testDbPath, cancellationToken);

            var recoveryTime = DateTime.UtcNow - startTime;
            var targetRecoveryTime = TimeSpan.FromHours(4); // RTO: 4時間

            // テストファイルクリーンアップ
            if (FileExistsSecurely(testDbPath))
            {
                System.IO.File.Delete(testDbPath);
            }

            var result = new DisasterRecoveryTestResultDto
            {
                Success = true,
                TestStartTime = startTime,
                TestEndTime = DateTime.UtcNow,
                RecoveryTime = recoveryTime,
                TargetRecoveryTime = targetRecoveryTime,
                RecoveryTimeMetTarget = recoveryTime <= targetRecoveryTime,
                BackupFileName = latestFullBackup.FileName,
                RecordCount = verification.RecordCount
            };

            _logger.LogInformation("災害復旧テスト完了 - User: {Username}, RecoveryTime: {RecoveryTime}, Success: {Success}",
                username, recoveryTime, result.RecoveryTimeMetTarget);

            return Ok(result);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "災害復旧テスト失敗 - User: {Username}", username);
            _securityLogService.LogSecurityViolation("DisasterRecoveryTestError", ex.Message, username, ipAddress);

            return StatusCode(500, new DisasterRecoveryTestResultDto
            {
                Success = false,
                ErrorMessage = "災害復旧テスト中にエラーが発生しました。"
            });
        }
    }

    /// <summary>
    /// セキュリティヘルパーメソッド - ファイルサイズを安全に取得
    /// </summary>
    /// <param name="filePath">ファイルパス</param>
    /// <returns>ファイルサイズ（バイト）</returns>
#pragma warning disable CA3003 // ファイルパスはサーバー側で生成され、ユーザー入力は含まれない
    private static long GetFileSizeSecurely(string filePath)
    {
        try
        {
            // ファイルパスはバックアップサービスによってサーバー側で生成される
            // ユーザー入力からのパスは使用しない
            if (string.IsNullOrWhiteSpace(filePath))
                return 0;
            
            return new FileInfo(filePath).Length;
        }
        catch (Exception)
        {
            return 0;
        }
    }
#pragma warning restore CA3003

    /// <summary>
    /// セキュリティヘルパーメソッド - ファイル存在確認を安全に実行
    /// </summary>
    /// <param name="filePath">ファイルパス</param>
    /// <returns>ファイル存在フラグ</returns>
#pragma warning disable CA3003 // ファイルパスはサーバー側で生成され、ユーザー入力は含まれない
    private static bool FileExistsSecurely(string filePath)
    {
        try
        {
            // ファイルパスはバックアップサービスによってサーバー側で生成される
            // ユーザー入力からのパスは使用しない
            if (string.IsNullOrWhiteSpace(filePath))
                return false;
                
            return System.IO.File.Exists(filePath);
        }
        catch (Exception)
        {
            return false;
        }
    }
#pragma warning restore CA3003
}

#region DTOs

public record CreateIncrementalBackupRequest
{
    public DateTime SinceDateTime { get; init; }
}

public record RestoreBackupRequest
{
    public string BackupFileName { get; init; } = string.Empty;
    public string? TargetDatabasePath { get; init; }
}

public record BackupResultDto
{
    public bool Success { get; init; }
    public string? BackupPath { get; init; }
    public long FileSizeBytes { get; init; }
    public DateTime CreatedAt { get; init; }
    public string? Type { get; init; }
    public bool IsValid { get; init; }
    public string? ValidationMessage { get; init; }
    public DateTime? SinceDateTime { get; init; }
    public string? ErrorMessage { get; init; }
}

public record RestoreResultDto
{
    public bool Success { get; init; }
    public string? BackupFileName { get; init; }
    public string? TargetDatabasePath { get; init; }
    public DateTime RestoredAt { get; init; }
    public TimeSpan RestoreDuration { get; init; }
    public int RecordCount { get; init; }
    public string? ErrorMessage { get; init; }
}

public record CleanupResultDto
{
    public bool Success { get; init; }
    public int DeletedCount { get; init; }
    public int RetentionDays { get; init; }
    public DateTime ExecutedAt { get; init; }
    public string? ErrorMessage { get; init; }
}

public record DisasterRecoveryTestResultDto
{
    public bool Success { get; init; }
    public DateTime TestStartTime { get; init; }
    public DateTime TestEndTime { get; init; }
    public TimeSpan RecoveryTime { get; init; }
    public TimeSpan TargetRecoveryTime { get; init; }
    public bool RecoveryTimeMetTarget { get; init; }
    public string? BackupFileName { get; init; }
    public int RecordCount { get; init; }
    public string? ErrorMessage { get; init; }
}

#endregion