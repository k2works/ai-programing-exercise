# ビルドステージ
FROM mcr.microsoft.com/dotnet/sdk:9.0-alpine AS build
WORKDIR /src

# プロジェクトファイルをコピーして復元
COPY ["src/ApiGateway/ApiGateway.fsproj", "src/ApiGateway/"]
RUN dotnet restore "src/ApiGateway/ApiGateway.fsproj"

# ソースコードをコピーしてビルド
COPY . .
WORKDIR "/src/src/ApiGateway"
RUN dotnet build "ApiGateway.fsproj" -c Release -o /app/build

# 発行ステージ
FROM build AS publish
RUN dotnet publish "ApiGateway.fsproj" -c Release -o /app/publish /p:UseAppHost=false

# 実行ステージ
FROM mcr.microsoft.com/dotnet/aspnet:9.0-alpine AS final
WORKDIR /app
EXPOSE 8080
COPY --from=publish /app/publish .
ENTRYPOINT ["dotnet", "ApiGateway.dll"]
