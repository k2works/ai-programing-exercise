# OpenAPI スキーマ生成スクリプト
# Swashbuckle.AspNetCore.Cli を使用して openapi.yaml を生成

param(
    [string]$OutputPath = "./openapi.yaml",
    [string]$ProjectPath = "../src/FinancialAccounting.Api/FinancialAccounting.Api.fsproj"
)

$ErrorActionPreference = "Stop"
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path

Write-Host "OpenAPI スキーマを生成中..." -ForegroundColor Cyan

# プロジェクトをビルド
Write-Host "プロジェクトをビルド中..."
$fullProjectPath = Join-Path $ScriptDir $ProjectPath
dotnet build $fullProjectPath -c Release --nologo -v q

if ($LASTEXITCODE -ne 0) {
    Write-Host "ビルドに失敗しました" -ForegroundColor Red
    exit 1
}

# Swashbuckle CLI をインストール（グローバルツール）
$toolInstalled = dotnet tool list -g | Select-String "swashbuckle.aspnetcore.cli"
if (-not $toolInstalled) {
    Write-Host "Swashbuckle CLI をインストール中..."
    dotnet tool install -g Swashbuckle.AspNetCore.Cli
}

# DLL パスを取得
$dllPath = Join-Path (Split-Path $fullProjectPath -Parent) "bin/Release/net9.0/FinancialAccounting.Api.dll"

if (-not (Test-Path $dllPath)) {
    Write-Host "DLL が見つかりません: $dllPath" -ForegroundColor Red
    exit 1
}

# OpenAPI JSON を生成
$jsonPath = Join-Path $ScriptDir "openapi.json"
Write-Host "OpenAPI JSON を生成中..."
swagger tofile --output $jsonPath $dllPath v1

if ($LASTEXITCODE -ne 0) {
    Write-Host "OpenAPI 生成に失敗しました" -ForegroundColor Red
    exit 1
}

# OpenAPI YAML を生成（swagger CLI の --yaml オプションを使用）
$yamlPath = Join-Path $ScriptDir $OutputPath
Write-Host "OpenAPI YAML を生成中..."
swagger tofile --output $yamlPath --yaml $dllPath v1

if ($LASTEXITCODE -ne 0) {
    Write-Host "YAML 生成に失敗しました。JSON ファイルは保持されます: $jsonPath" -ForegroundColor Yellow
    exit 1
}

Write-Host ""
Write-Host "OpenAPI スキーマを生成しました:" -ForegroundColor Green
Write-Host "  YAML: $yamlPath" -ForegroundColor Green
Write-Host "  JSON: $jsonPath" -ForegroundColor Green
