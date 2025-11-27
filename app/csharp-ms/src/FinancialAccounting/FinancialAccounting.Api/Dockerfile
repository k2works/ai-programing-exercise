# Build Stage
FROM mcr.microsoft.com/dotnet/sdk:10.0-preview AS build
WORKDIR /src

# Copy project files
COPY ["src/FinancialAccounting/FinancialAccounting.Api/FinancialAccounting.Api.csproj", "src/FinancialAccounting/FinancialAccounting.Api/"]
COPY ["src/FinancialAccounting/FinancialAccounting.Application/FinancialAccounting.Application.csproj", "src/FinancialAccounting/FinancialAccounting.Application/"]
COPY ["src/FinancialAccounting/FinancialAccounting.Domain/FinancialAccounting.Domain.csproj", "src/FinancialAccounting/FinancialAccounting.Domain/"]
COPY ["src/FinancialAccounting/FinancialAccounting.Infrastructure/FinancialAccounting.Infrastructure.csproj", "src/FinancialAccounting/FinancialAccounting.Infrastructure/"]

# Restore
RUN dotnet restore "src/FinancialAccounting/FinancialAccounting.Api/FinancialAccounting.Api.csproj"

# Copy source code
COPY src/FinancialAccounting/ src/FinancialAccounting/

# Build
WORKDIR "/src/src/FinancialAccounting/FinancialAccounting.Api"
RUN dotnet build "FinancialAccounting.Api.csproj" -c Release -o /app/build

# Publish
FROM build AS publish
RUN dotnet publish "FinancialAccounting.Api.csproj" -c Release -o /app/publish /p:UseAppHost=false

# Runtime Stage
FROM mcr.microsoft.com/dotnet/aspnet:10.0-preview AS runtime
WORKDIR /app
EXPOSE 8080

COPY --from=publish /app/publish .
ENTRYPOINT ["dotnet", "FinancialAccounting.Api.dll"]
