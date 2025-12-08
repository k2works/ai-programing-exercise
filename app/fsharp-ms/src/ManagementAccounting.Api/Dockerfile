# ビルドステージ
FROM mcr.microsoft.com/dotnet/sdk:9.0-alpine AS build
WORKDIR /src

# プロジェクトファイルをコピーして復元
COPY ["src/ManagementAccounting.Api/ManagementAccounting.Api.fsproj", "src/ManagementAccounting.Api/"]
COPY ["src/ManagementAccounting.Domain/ManagementAccounting.Domain.fsproj", "src/ManagementAccounting.Domain/"]
COPY ["src/ManagementAccounting.Application/ManagementAccounting.Application.fsproj", "src/ManagementAccounting.Application/"]
COPY ["src/ManagementAccounting.Infrastructure/ManagementAccounting.Infrastructure.fsproj", "src/ManagementAccounting.Infrastructure/"]
COPY ["src/Shared.Contracts/Shared.Contracts.fsproj", "src/Shared.Contracts/"]
RUN dotnet restore "src/ManagementAccounting.Api/ManagementAccounting.Api.fsproj"

# ソースコードをコピーしてビルド
COPY . .
WORKDIR "/src/src/ManagementAccounting.Api"
RUN dotnet build "ManagementAccounting.Api.fsproj" -c Release -o /app/build

# 発行ステージ
FROM build AS publish
RUN dotnet publish "ManagementAccounting.Api.fsproj" -c Release -o /app/publish /p:UseAppHost=false

# 実行ステージ
FROM mcr.microsoft.com/dotnet/aspnet:9.0-alpine AS final
WORKDIR /app
EXPOSE 8080
COPY --from=publish /app/publish .
ENTRYPOINT ["dotnet", "ManagementAccounting.Api.dll"]
