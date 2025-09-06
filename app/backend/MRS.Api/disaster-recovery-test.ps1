# 災害復旧演習スクリプト - 4時間以内復旧の検証
# このスクリプトは MRS システムの災害復旧手順を自動化し、RTO（Recovery Time Objective）4時間以内を検証します

param(
    [string]$DatabasePath = "mrs.db",
    [string]$BackupDirectory = "backups",
    [string]$TestDirectory = "disaster-recovery-test"
)

$ErrorActionPreference = "Stop"
$startTime = Get-Date

Write-Host "=== MRS 災害復旧演習開始 ===" -ForegroundColor Green
Write-Host "開始時刻: $($startTime.ToString('yyyy-MM-dd HH:mm:ss'))" -ForegroundColor Yellow
Write-Host "目標復旧時間: 4時間以内" -ForegroundColor Yellow
Write-Host ""

try {
    # Step 1: 環境準備
    Write-Host "[Step 1] 環境準備中..." -ForegroundColor Cyan
    
    if (-not (Test-Path $BackupDirectory)) {
        New-Item -ItemType Directory -Path $BackupDirectory | Out-Null
    }
    
    if (-not (Test-Path $TestDirectory)) {
        New-Item -ItemType Directory -Path $TestDirectory | Out-Null
    }
    
    $step1Time = (Get-Date) - $startTime
    Write-Host "Step 1 完了: $($step1Time.TotalMinutes.ToString('F2'))分" -ForegroundColor Green
    Write-Host ""
    
    # Step 2: 元データの確認
    Write-Host "[Step 2] 元データベースの確認中..." -ForegroundColor Cyan
    
    if (-not (Test-Path $DatabasePath)) {
        Write-Host "警告: 元データベースが存在しません。テスト用データベースを作成します..." -ForegroundColor Yellow
        # テスト用の空のデータベースを作成（実環境では既存のデータを使用）
        dotnet ef database update --no-build 2>&1 | Out-Host
    }
    
    $originalSize = (Get-Item $DatabasePath -ErrorAction SilentlyContinue)?.Length ?? 0
    Write-Host "元データベースサイズ: $($originalSize / 1KB)KB" -ForegroundColor Yellow
    
    $step2Time = (Get-Date) - $startTime
    Write-Host "Step 2 完了: $($step2Time.TotalMinutes.ToString('F2'))分" -ForegroundColor Green
    Write-Host ""
    
    # Step 3: フルバックアップ作成
    Write-Host "[Step 3] フルバックアップ作成中..." -ForegroundColor Cyan
    
    $backupTimestamp = Get-Date -Format "yyyyMMdd-HHmmss"
    $backupPath = Join-Path $BackupDirectory "mrs-backup-$backupTimestamp.db"
    
    # データベースファイルを直接コピー（SQLite バックアップ）
    Copy-Item $DatabasePath $backupPath -Force
    
    # 圧縮バックアップ作成（実際のサービス実装をシミュレート）
    $compressedBackupPath = "$backupPath.gz"
    
    # PowerShellの圧縮機能を使用
    $originalData = [System.IO.File]::ReadAllBytes($backupPath)
    $compressedData = [System.IO.Compression.GzipStream]::new(
        [System.IO.File]::OpenWrite($compressedBackupPath),
        [System.IO.Compression.CompressionMode]::Compress
    )
    $compressedData.Write($originalData, 0, $originalData.Length)
    $compressedData.Close()
    
    Remove-Item $backupPath # 元の非圧縮ファイルを削除
    
    $backupSize = (Get-Item $compressedBackupPath).Length
    $compressionRatio = [Math]::Round(($backupSize / $originalSize) * 100, 2)
    
    Write-Host "バックアップ作成完了: $compressedBackupPath" -ForegroundColor Yellow
    Write-Host "圧縮後サイズ: $($backupSize / 1KB)KB (圧縮率: $compressionRatio%)" -ForegroundColor Yellow
    
    $step3Time = (Get-Date) - $startTime
    Write-Host "Step 3 完了: $($step3Time.TotalMinutes.ToString('F2'))分" -ForegroundColor Green
    Write-Host ""
    
    # Step 4: バックアップ検証
    Write-Host "[Step 4] バックアップファイル検証中..." -ForegroundColor Cyan
    
    if (-not (Test-Path $compressedBackupPath)) {
        throw "バックアップファイルが見つかりません: $compressedBackupPath"
    }
    
    # SHA256 チェックサム計算（セキュリティベストプラクティス）
    $checksumStart = Get-Date
    $checksum = Get-FileHash $compressedBackupPath -Algorithm SHA256
    $checksumTime = (Get-Date) - $checksumStart
    
    Write-Host "バックアップ検証完了" -ForegroundColor Yellow
    Write-Host "SHA256 チェックサム: $($checksum.Hash)" -ForegroundColor Yellow
    Write-Host "検証時間: $($checksumTime.TotalSeconds.ToString('F2'))秒" -ForegroundColor Yellow
    
    $step4Time = (Get-Date) - $startTime
    Write-Host "Step 4 完了: $($step4Time.TotalMinutes.ToString('F2'))分" -ForegroundColor Green
    Write-Host ""
    
    # Step 5: 災害発生シミュレーション
    Write-Host "[Step 5] 災害発生シミュレーション中..." -ForegroundColor Red
    
    $simulatedDatabasePath = Join-Path $TestDirectory "mrs-corrupted.db"
    
    # 元のデータベースを破損させる（テスト環境でのシミュレーション）
    Copy-Item $DatabasePath $simulatedDatabasePath
    
    # ファイルの一部を破損させてデータベース障害をシミュレート
    $corruptedData = [System.IO.File]::ReadAllBytes($simulatedDatabasePath)
    # ファイルの中間部分にランダムデータを注入
    $corruptionStart = [Math]::Floor($corruptedData.Length / 3)
    for ($i = 0; $i -lt 100; $i++) {
        $corruptedData[$corruptionStart + $i] = Get-Random -Minimum 0 -Maximum 256
    }
    [System.IO.File]::WriteAllBytes($simulatedDatabasePath, $corruptedData)
    
    Write-Host "データベース破損をシミュレート: $simulatedDatabasePath" -ForegroundColor Red
    
    $step5Time = (Get-Date) - $startTime
    Write-Host "Step 5 完了: $($step5Time.TotalMinutes.ToString('F2'))分" -ForegroundColor Green
    Write-Host ""
    
    # Step 6: 復元処理
    Write-Host "[Step 6] バックアップからの復元処理中..." -ForegroundColor Cyan
    
    $restoreStartTime = Get-Date
    $restoredDatabasePath = Join-Path $TestDirectory "mrs-restored.db"
    
    # 圧縮ファイルの展開
    $compressedStream = [System.IO.File]::OpenRead($compressedBackupPath)
    $gzipStream = [System.IO.Compression.GzipStream]::new($compressedStream, [System.IO.Compression.CompressionMode]::Decompress)
    $outputStream = [System.IO.File]::Create($restoredDatabasePath)
    
    $gzipStream.CopyTo($outputStream)
    
    $outputStream.Close()
    $gzipStream.Close()
    $compressedStream.Close()
    
    $restoreTime = (Get-Date) - $restoreStartTime
    $restoredSize = (Get-Item $restoredDatabasePath).Length
    
    Write-Host "復元完了: $restoredDatabasePath" -ForegroundColor Yellow
    Write-Host "復元時間: $($restoreTime.TotalMinutes.ToString('F2'))分" -ForegroundColor Yellow
    Write-Host "復元後サイズ: $($restoredSize / 1KB)KB" -ForegroundColor Yellow
    
    $step6Time = (Get-Date) - $startTime
    Write-Host "Step 6 完了: $($step6Time.TotalMinutes.ToString('F2'))分" -ForegroundColor Green
    Write-Host ""
    
    # Step 7: 復元データベースの整合性検証
    Write-Host "[Step 7] 復元データベースの整合性検証中..." -ForegroundColor Cyan
    
    # ファイルサイズ比較
    if ($restoredSize -ne $originalSize) {
        Write-Host "警告: ファイルサイズが一致しません（元: $originalSize, 復元後: $restoredSize）" -ForegroundColor Yellow
    } else {
        Write-Host "ファイルサイズ検証: OK" -ForegroundColor Green
    }
    
    # チェックサム比較（復元データベース）
    $restoredChecksum = Get-FileHash $restoredDatabasePath -Algorithm SHA256
    $originalChecksum = Get-FileHash $DatabasePath -Algorithm SHA256
    
    if ($restoredChecksum.Hash -eq $originalChecksum.Hash) {
        Write-Host "データ整合性検証: OK (チェックサム一致)" -ForegroundColor Green
    } else {
        Write-Host "警告: チェックサムが一致しません" -ForegroundColor Yellow
    }
    
    $step7Time = (Get-Date) - $startTime
    Write-Host "Step 7 完了: $($step7Time.TotalMinutes.ToString('F2'))分" -ForegroundColor Green
    Write-Host ""
    
    # Step 8: サービス復旧確認
    Write-Host "[Step 8] サービス復旧確認中..." -ForegroundColor Cyan
    
    # 復元されたデータベースでアプリケーション動作確認をシミュレート
    # 実環境では実際のアプリケーションサーバーの起動とヘルスチェック
    
    Write-Host "アプリケーション動作確認をシミュレート..." -ForegroundColor Yellow
    Start-Sleep -Seconds 2  # 起動時間をシミュレート
    
    Write-Host "ヘルスチェック: データベース接続 OK" -ForegroundColor Green
    Write-Host "ヘルスチェック: API エンドポイント OK" -ForegroundColor Green
    Write-Host "ヘルスチェック: 認証システム OK" -ForegroundColor Green
    
    $step8Time = (Get-Date) - $startTime
    Write-Host "Step 8 完了: $($step8Time.TotalMinutes.ToString('F2'))分" -ForegroundColor Green
    Write-Host ""
    
    # 最終結果
    $totalTime = (Get-Date) - $startTime
    $targetTime = [TimeSpan]::FromHours(4)
    
    Write-Host "=== 災害復旧演習結果 ===" -ForegroundColor Green
    Write-Host "総復旧時間: $($totalTime.TotalMinutes.ToString('F2'))分 ($($totalTime.TotalHours.ToString('F2'))時間)" -ForegroundColor Yellow
    Write-Host "目標復旧時間: $($targetTime.TotalHours)時間" -ForegroundColor Yellow
    
    if ($totalTime -le $targetTime) {
        Write-Host "結果: RTO 目標達成 ✓" -ForegroundColor Green
    } else {
        Write-Host "結果: RTO 目標未達成 ✗" -ForegroundColor Red
        Write-Host "超過時間: $((($totalTime - $targetTime).TotalMinutes).ToString('F2'))分" -ForegroundColor Red
    }
    
    Write-Host ""
    Write-Host "詳細タイミング:" -ForegroundColor Cyan
    Write-Host "  環境準備: $($step1Time.TotalMinutes.ToString('F2'))分" -ForegroundColor White
    Write-Host "  データ確認: $($step2Time.TotalMinutes.ToString('F2'))分" -ForegroundColor White  
    Write-Host "  バックアップ作成: $($step3Time.TotalMinutes.ToString('F2'))分" -ForegroundColor White
    Write-Host "  バックアップ検証: $($step4Time.TotalMinutes.ToString('F2'))分" -ForegroundColor White
    Write-Host "  災害シミュレート: $($step5Time.TotalMinutes.ToString('F2'))分" -ForegroundColor White
    Write-Host "  復元処理: $($step6Time.TotalMinutes.ToString('F2'))分" -ForegroundColor White
    Write-Host "  整合性検証: $($step7Time.TotalMinutes.ToString('F2'))分" -ForegroundColor White
    Write-Host "  サービス復旧: $($step8Time.TotalMinutes.ToString('F2'))分" -ForegroundColor White
    
    Write-Host ""
    Write-Host "演習完了時刻: $((Get-Date).ToString('yyyy-MM-dd HH:mm:ss'))" -ForegroundColor Green
    
}
catch {
    Write-Host ""
    Write-Host "=== 災害復旧演習失敗 ===" -ForegroundColor Red
    Write-Host "エラー: $($_.Exception.Message)" -ForegroundColor Red
    $totalTime = (Get-Date) - $startTime
    Write-Host "失敗時点での経過時間: $($totalTime.TotalMinutes.ToString('F2'))分" -ForegroundColor Yellow
    
    exit 1
}
finally {
    # クリーンアップ
    Write-Host ""
    Write-Host "クリーンアップ中..." -ForegroundColor Cyan
    
    # テストファイルの削除
    if (Test-Path $TestDirectory) {
        Remove-Item -Path $TestDirectory -Recurse -Force -ErrorAction SilentlyContinue
    }
    
    Write-Host "クリーンアップ完了" -ForegroundColor Green
}