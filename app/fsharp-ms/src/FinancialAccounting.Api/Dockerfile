# ビルドステージ
FROM mcr.microsoft.com/dotnet/sdk:9.0-alpine AS build
WORKDIR /src

# プロジェクトファイルをコピーして復元
COPY ["src/FinancialAccounting.Api/FinancialAccounting.Api.fsproj", "src/FinancialAccounting.Api/"]
COPY ["src/FinancialAccounting.Domain/FinancialAccounting.Domain.fsproj", "src/FinancialAccounting.Domain/"]
COPY ["src/FinancialAccounting.Application/FinancialAccounting.Application.fsproj", "src/FinancialAccounting.Application/"]
COPY ["src/FinancialAccounting.Infrastructure/FinancialAccounting.Infrastructure.fsproj", "src/FinancialAccounting.Infrastructure/"]
COPY ["src/Shared.Contracts/Shared.Contracts.fsproj", "src/Shared.Contracts/"]
RUN dotnet restore "src/FinancialAccounting.Api/FinancialAccounting.Api.fsproj"

# ソースコードをコピーしてビルド
COPY . .
WORKDIR "/src/src/FinancialAccounting.Api"
RUN dotnet build "FinancialAccounting.Api.fsproj" -c Release -o /app/build

# 発行ステージ
FROM build AS publish
RUN dotnet publish "FinancialAccounting.Api.fsproj" -c Release -o /app/publish /p:UseAppHost=false

# 実行ステージ
FROM mcr.microsoft.com/dotnet/aspnet:9.0-alpine AS final
WORKDIR /app
EXPOSE 8080
COPY --from=publish /app/publish .
ENTRYPOINT ["dotnet", "FinancialAccounting.Api.dll"]
