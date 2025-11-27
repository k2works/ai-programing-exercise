# Build Stage
FROM mcr.microsoft.com/dotnet/sdk:10.0-preview AS build
WORKDIR /src

# Copy project files
COPY ["src/ApiGateway/ApiGateway/ApiGateway.csproj", "src/ApiGateway/ApiGateway/"]

# Restore
RUN dotnet restore "src/ApiGateway/ApiGateway/ApiGateway.csproj"

# Copy source code
COPY src/ApiGateway/ src/ApiGateway/

# Build
WORKDIR "/src/src/ApiGateway/ApiGateway"
RUN dotnet build "ApiGateway.csproj" -c Release -o /app/build

# Publish
FROM build AS publish
RUN dotnet publish "ApiGateway.csproj" -c Release -o /app/publish /p:UseAppHost=false

# Runtime Stage
FROM mcr.microsoft.com/dotnet/aspnet:10.0-preview AS runtime
WORKDIR /app
EXPOSE 8080

COPY --from=publish /app/publish .
ENTRYPOINT ["dotnet", "ApiGateway.dll"]
