# Build Stage
FROM mcr.microsoft.com/dotnet/sdk:10.0-preview AS build
WORKDIR /src

# Copy project files
COPY ["src/ManagementAccounting/ManagementAccounting.Api/ManagementAccounting.Api.csproj", "src/ManagementAccounting/ManagementAccounting.Api/"]
COPY ["src/ManagementAccounting/ManagementAccounting.Application/ManagementAccounting.Application.csproj", "src/ManagementAccounting/ManagementAccounting.Application/"]
COPY ["src/ManagementAccounting/ManagementAccounting.Domain/ManagementAccounting.Domain.csproj", "src/ManagementAccounting/ManagementAccounting.Domain/"]
COPY ["src/ManagementAccounting/ManagementAccounting.Infrastructure/ManagementAccounting.Infrastructure.csproj", "src/ManagementAccounting/ManagementAccounting.Infrastructure/"]

# Restore
RUN dotnet restore "src/ManagementAccounting/ManagementAccounting.Api/ManagementAccounting.Api.csproj"

# Copy source code
COPY src/ManagementAccounting/ src/ManagementAccounting/

# Build
WORKDIR "/src/src/ManagementAccounting/ManagementAccounting.Api"
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
