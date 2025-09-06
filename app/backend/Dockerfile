# Build stage
FROM mcr.microsoft.com/dotnet/sdk:9.0 AS build
WORKDIR /src

# Copy project files and restore dependencies
COPY ["MRS.Api/MRS.Api.csproj", "MRS.Api/"]
COPY ["MRS.Application/MRS.Application.csproj", "MRS.Application/"]
COPY ["MRS.Domain/MRS.Domain.csproj", "MRS.Domain/"]
COPY ["MRS.Infrastructure/MRS.Infrastructure.csproj", "MRS.Infrastructure/"]

RUN dotnet restore "MRS.Api/MRS.Api.csproj"

# Copy all source files
COPY . .

# Build the application
WORKDIR "/src/MRS.Api"
RUN dotnet build "MRS.Api.csproj" -c Release -o /app/build

# Publish stage
FROM build AS publish
RUN dotnet publish "MRS.Api.csproj" -c Release -o /app/publish /p:UseAppHost=false

# Runtime stage
FROM mcr.microsoft.com/dotnet/aspnet:9.0 AS final

# Create non-root user
RUN groupadd -r appuser && useradd -r -g appuser appuser

# Set working directory
WORKDIR /app

# Copy published application
COPY --from=publish /app/publish .

# Create necessary directories and set permissions
RUN mkdir -p /app/logs /app/backups /app/data && \
    chown -R appuser:appuser /app && \
    chmod -R 755 /app

# Install SQLite for database operations
RUN apt-get update && \
    apt-get install -y sqlite3 && \
    rm -rf /var/lib/apt/lists/*

# Switch to non-root user
USER appuser

# Environment variables
ENV ASPNETCORE_ENVIRONMENT=Production
ENV ASPNETCORE_URLS=http://+:8080
ENV ASPNETCORE_HTTP_PORTS=8080

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
    CMD curl --fail http://localhost:8080/health || exit 1

# Expose port
EXPOSE 8080

# Entry point
ENTRYPOINT ["dotnet", "MRS.Api.dll"]