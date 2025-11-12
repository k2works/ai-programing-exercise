# OpenAPI 仕様を生成する PowerShell スクリプト

$ErrorActionPreference = "Stop"

Write-Host "🔨 Building the API project..." -ForegroundColor Cyan
dotnet build SalesManagement.Api/SalesManagement.Api.fsproj

Write-Host "📝 Generating OpenAPI specification..." -ForegroundColor Cyan
dotnet swagger tofile --output SalesManagement.Api/openapi.yml --yaml SalesManagement.Api/bin/Debug/net9.0/SalesManagement.Api.dll v1

Write-Host "🔧 Fixing OpenAPI version to 3.0.1 for better tool compatibility..." -ForegroundColor Cyan
Start-Sleep -Milliseconds 500
$content = Get-Content SalesManagement.Api/openapi.yml -Raw
$content = $content -replace 'openapi: 3\.0\.\d+', 'openapi: 3.0.1'
$content | Set-Content SalesManagement.Api/openapi.yml -NoNewline

Write-Host "✅ OpenAPI specification generated successfully!" -ForegroundColor Green
Write-Host "📄 Output: SalesManagement.Api/openapi.yml" -ForegroundColor Yellow
