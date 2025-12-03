# Multi-stage build for Management Accounting Service

# Stage 1: Build
FROM sbtscala/scala-sbt:eclipse-temurin-jammy-21.0.2_13_1.9.9_3.3.3 AS builder

WORKDIR /app

# Copy build files
COPY build.sbt .
COPY project/ project/
COPY common/ common/
COPY management-accounting-service/ management-accounting-service/

# Build the application
RUN sbt "project managementAccountingService" "Universal / packageBin"

# Stage 2: Runtime
FROM eclipse-temurin:21-jre

WORKDIR /app

# Install curl and unzip for health checks and extraction
RUN apt-get update && apt-get install -y curl unzip && rm -rf /var/lib/apt/lists/*

# Copy the built application
COPY --from=builder /app/management-accounting-service/target/universal/*.zip /app/
RUN unzip *.zip && rm *.zip && mv management-accounting-service-*/* . && rm -rf management-accounting-service-*

# Expose port
EXPOSE 8081

# Run the application
CMD ["./bin/management-accounting-service"]
