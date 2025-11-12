# OpenAPI 仕様を生成する PowerShell スクリプト

$ErrorActionPreference = "Stop"

Write-Host "🔨 Building the API project..." -ForegroundColor Cyan
dotnet build SalesManagement.Api/SalesManagement.Api.fsproj

Write-Host "📝 Generating OpenAPI specification..." -ForegroundColor Cyan
dotnet swagger tofile --output SalesManagement.Api/openapi.yml --yaml SalesManagement.Api/bin/Debug/net9.0/SalesManagement.Api.dll v1

Write-Host "✅ OpenAPI specification generated successfully!" -ForegroundColor Green
Write-Host "📄 Output: SalesManagement.Api/openapi.yml" -ForegroundColor Yellow
