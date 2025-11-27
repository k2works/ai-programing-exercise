# Build Stage
FROM mcr.microsoft.com/dotnet/sdk:10.0-preview AS build
WORKDIR /src

# Copy project files
COPY ["FinancialAccounting/FinancialAccounting.Api/FinancialAccounting.Api.csproj", "FinancialAccounting/FinancialAccounting.Api/"]
COPY ["FinancialAccounting/FinancialAccounting.Application/FinancialAccounting.Application.csproj", "FinancialAccounting/FinancialAccounting.Application/"]
COPY ["FinancialAccounting/FinancialAccounting.Domain/FinancialAccounting.Domain.csproj", "FinancialAccounting/FinancialAccounting.Domain/"]
COPY ["FinancialAccounting/FinancialAccounting.Infrastructure/FinancialAccounting.Infrastructure.csproj", "FinancialAccounting/FinancialAccounting.Infrastructure/"]
COPY ["Shared/Shared.Contracts/Shared.Contracts.csproj", "Shared/Shared.Contracts/"]

# Restore
RUN dotnet restore "FinancialAccounting/FinancialAccounting.Api/FinancialAccounting.Api.csproj"

# Copy source code
COPY FinancialAccounting/ FinancialAccounting/
COPY Shared/ Shared/

# Build
WORKDIR "/src/FinancialAccounting/FinancialAccounting.Api"
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
