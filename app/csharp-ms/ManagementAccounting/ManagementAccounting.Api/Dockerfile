# Build Stage
FROM mcr.microsoft.com/dotnet/sdk:10.0-preview AS build
WORKDIR /src

# Copy project files
COPY ["ManagementAccounting/ManagementAccounting.Api/ManagementAccounting.Api.csproj", "ManagementAccounting/ManagementAccounting.Api/"]
COPY ["ManagementAccounting/ManagementAccounting.Application/ManagementAccounting.Application.csproj", "ManagementAccounting/ManagementAccounting.Application/"]
COPY ["ManagementAccounting/ManagementAccounting.Domain/ManagementAccounting.Domain.csproj", "ManagementAccounting/ManagementAccounting.Domain/"]
COPY ["ManagementAccounting/ManagementAccounting.Infrastructure/ManagementAccounting.Infrastructure.csproj", "ManagementAccounting/ManagementAccounting.Infrastructure/"]
COPY ["Shared/Shared.Contracts/Shared.Contracts.csproj", "Shared/Shared.Contracts/"]

# Restore
RUN dotnet restore "ManagementAccounting/ManagementAccounting.Api/ManagementAccounting.Api.csproj"

# Copy source code
COPY ManagementAccounting/ ManagementAccounting/
COPY Shared/ Shared/

# Build
WORKDIR "/src/ManagementAccounting/ManagementAccounting.Api"
RUN dotnet build "ManagementAccounting.Api.csproj" -c Release -o /app/build

# Publish
FROM build AS publish
RUN dotnet publish "ManagementAccounting.Api.csproj" -c Release -o /app/publish /p:UseAppHost=false

# Runtime Stage
FROM mcr.microsoft.com/dotnet/aspnet:10.0-preview AS runtime
WORKDIR /app
EXPOSE 8080

COPY --from=publish /app/publish .
ENTRYPOINT ["dotnet", "ManagementAccounting.Api.dll"]
