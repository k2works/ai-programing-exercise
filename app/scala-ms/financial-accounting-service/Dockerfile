# Multi-stage build for Financial Accounting Service

# Stage 1: Build
FROM sbtscala/scala-sbt:eclipse-temurin-jammy-21.0.2_13_1.9.9_3.3.3 AS builder

WORKDIR /app

# Copy build files
COPY build.sbt .
COPY project/ project/
COPY common/ common/
COPY financial-accounting-service/ financial-accounting-service/

# Build the application
RUN sbt "project financialAccountingService" "Universal / packageBin"

# Stage 2: Runtime
FROM eclipse-temurin:21-jre

WORKDIR /app

# Install curl for health checks
RUN apt-get update && apt-get install -y curl && rm -rf /var/lib/apt/lists/*

# Copy the built application
COPY --from=builder /app/financial-accounting-service/target/universal/*.zip /app/
RUN unzip *.zip && rm *.zip && mv financial-accounting-service-*/* . && rm -rf financial-accounting-service-*

# Expose port
EXPOSE 8080

# Run the application
CMD ["./bin/financial-accounting-service"]
