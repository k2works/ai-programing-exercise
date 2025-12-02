# Multi-stage build for API Gateway

# Stage 1: Build
FROM sbtscala/scala-sbt:eclipse-temurin-21.0.2_13_1.9.9_3.3.3 AS builder

WORKDIR /app

# Copy build files
COPY build.sbt .
COPY project/ project/
COPY common/ common/
COPY api-gateway/ api-gateway/

# Build the application
RUN sbt "project apiGateway" "Universal / packageBin"

# Stage 2: Runtime
FROM eclipse-temurin:21-jre

WORKDIR /app

# Install curl for health checks
RUN apt-get update && apt-get install -y curl && rm -rf /var/lib/apt/lists/*

# Copy the built application
COPY --from=builder /app/api-gateway/target/universal/*.zip /app/
RUN unzip *.zip && rm *.zip && mv api-gateway-*/* . && rm -rf api-gateway-*

# Expose port
EXPOSE 8000

# Run the application
CMD ["./bin/api-gateway"]
