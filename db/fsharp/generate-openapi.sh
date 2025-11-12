#!/bin/bash
# OpenAPI 仕様を生成するスクリプト

set -e

echo "🔨 Building the API project..."
dotnet build SalesManagement.Api/SalesManagement.Api.fsproj

echo "📝 Generating OpenAPI specification..."
dotnet swagger tofile --output SalesManagement.Api/openapi.yml --yaml SalesManagement.Api/bin/Debug/net9.0/SalesManagement.Api.dll v1

echo "🔧 Fixing OpenAPI version to 3.0.1 for better tool compatibility..."
sed -i 's/openapi: 3\.0\.[0-9]\+/openapi: 3.0.1/' SalesManagement.Api/openapi.yml

echo "✅ OpenAPI specification generated successfully!"
echo "📄 Output: SalesManagement.Api/openapi.yml"
