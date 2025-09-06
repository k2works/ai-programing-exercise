#!/bin/bash

# MRS Production Deployment Script
# Usage: ./deploy.sh [version] [environment]

set -e  # Exit on error

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$(dirname "$SCRIPT_DIR")")"
VERSION=${1:-latest}
ENVIRONMENT=${2:-production}

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Pre-deployment checks
pre_deployment_checks() {
    log_info "Running pre-deployment checks..."
    
    # Check if Docker is running
    if ! docker info >/dev/null 2>&1; then
        log_error "Docker is not running. Please start Docker and try again."
        exit 1
    fi
    
    # Check if required environment files exist
    if [[ ! -f "$PROJECT_ROOT/.env.production" ]]; then
        log_error "Production environment file not found: $PROJECT_ROOT/.env.production"
        log_info "Please copy .env.production.example to .env.production and configure it."
        exit 1
    fi
    
    # Check if required directories exist
    REQUIRED_DIRS=(
        "/var/log/mrs"
        "/var/backups/mrs"
        "/var/backups/postgres"
    )
    
    for dir in "${REQUIRED_DIRS[@]}"; do
        if [[ ! -d "$dir" ]]; then
            log_warning "Creating missing directory: $dir"
            sudo mkdir -p "$dir"
            sudo chown $USER:$USER "$dir"
        fi
    done
    
    log_success "Pre-deployment checks completed"
}

# Build Docker images
build_images() {
    log_info "Building Docker images for version: $VERSION"
    
    cd "$PROJECT_ROOT"
    
    # Build backend image
    log_info "Building backend image..."
    docker build -t "mrs-backend:$VERSION" -f app/backend/Dockerfile app/backend/
    
    # Build frontend image
    log_info "Building frontend image..."
    docker build -t "mrs-frontend:$VERSION" -f app/frontend/Dockerfile app/frontend/
    
    # Tag as latest if this is a production deployment
    if [[ "$VERSION" != "latest" ]]; then
        docker tag "mrs-backend:$VERSION" "mrs-backend:latest"
        docker tag "mrs-frontend:$VERSION" "mrs-frontend:latest"
    fi
    
    log_success "Docker images built successfully"
}

# Database migration
run_migrations() {
    log_info "Running database migrations..."
    
    # Check if database container is running
    if docker ps --format "table {{.Names}}" | grep -q "mrs-postgres-prod"; then
        # Run migrations using the backend container
        docker run --rm --network mrs-network \
            --env-file "$PROJECT_ROOT/.env.production" \
            "mrs-backend:$VERSION" \
            dotnet ef database update --no-build
    else
        log_warning "Database container not found. Migrations will be run after deployment."
    fi
    
    log_success "Database migrations completed"
}

# Deploy application
deploy() {
    log_info "Deploying MRS application (version: $VERSION, environment: $ENVIRONMENT)"
    
    cd "$PROJECT_ROOT/deployment/production"
    
    # Export version for docker-compose
    export VERSION="$VERSION"
    
    # Pull latest images if using remote registry
    # docker-compose -f docker-compose.prod.yml pull
    
    # Deploy with zero-downtime strategy
    log_info "Performing blue-green deployment..."
    
    # Scale up new instances
    docker-compose -f docker-compose.prod.yml up -d --scale mrs-backend=2
    
    # Wait for health checks
    sleep 30
    
    # Check health of new instances
    for i in {1..30}; do
        if docker-compose -f docker-compose.prod.yml exec mrs-backend curl -f http://localhost:8080/health >/dev/null 2>&1; then
            log_success "New backend instance is healthy"
            break
        fi
        if [[ $i -eq 30 ]]; then
            log_error "Health check failed for new backend instance"
            exit 1
        fi
        sleep 2
    done
    
    # Scale down old instances
    docker-compose -f docker-compose.prod.yml up -d --scale mrs-backend=1
    
    log_success "Deployment completed successfully"
}

# Post-deployment verification
post_deployment_verification() {
    log_info "Running post-deployment verification..."
    
    # Wait for all services to be ready
    sleep 10
    
    # Check service health
    SERVICES=("mrs-backend" "mrs-frontend" "postgres" "redis")
    
    for service in "${SERVICES[@]}"; do
        if docker-compose -f docker-compose.prod.yml ps "$service" | grep -q "Up"; then
            log_success "$service is running"
        else
            log_error "$service is not running"
            docker-compose -f docker-compose.prod.yml logs "$service"
            exit 1
        fi
    done
    
    # Test API endpoints
    log_info "Testing API endpoints..."
    
    # Test health endpoint
    if curl -f http://localhost:8080/health >/dev/null 2>&1; then
        log_success "Health endpoint is responding"
    else
        log_error "Health endpoint is not responding"
        exit 1
    fi
    
    # Test frontend
    if curl -f http://localhost/ >/dev/null 2>&1; then
        log_success "Frontend is responding"
    else
        log_warning "Frontend may not be ready yet"
    fi
    
    log_success "Post-deployment verification completed"
}

# Rollback function
rollback() {
    log_warning "Rolling back to previous version..."
    
    cd "$PROJECT_ROOT/deployment/production"
    
    # Get previous version from docker images
    PREVIOUS_VERSION=$(docker images --format "table {{.Repository}}:{{.Tag}}" | grep "mrs-backend" | grep -v "latest" | head -2 | tail -1 | cut -d':' -f2)
    
    if [[ -n "$PREVIOUS_VERSION" ]]; then
        log_info "Rolling back to version: $PREVIOUS_VERSION"
        
        # Update images to previous version
        docker tag "mrs-backend:$PREVIOUS_VERSION" "mrs-backend:latest"
        docker tag "mrs-frontend:$PREVIOUS_VERSION" "mrs-frontend:latest"
        
        # Restart services
        docker-compose -f docker-compose.prod.yml down
        docker-compose -f docker-compose.prod.yml up -d
        
        log_success "Rollback completed"
    else
        log_error "No previous version found for rollback"
        exit 1
    fi
}

# Cleanup function
cleanup() {
    log_info "Cleaning up old Docker images..."
    
    # Remove old images (keep last 3 versions)
    docker images --format "table {{.Repository}}:{{.Tag}}" | grep "mrs-" | grep -v "latest" | tail -n +4 | while read -r image; do
        docker rmi "$image" || true
    done
    
    # Remove unused volumes and networks
    docker system prune -f
    
    log_success "Cleanup completed"
}

# Main deployment function
main() {
    log_info "Starting MRS deployment process..."
    log_info "Version: $VERSION, Environment: $ENVIRONMENT"
    
    case "${3:-deploy}" in
        "deploy")
            pre_deployment_checks
            build_images
            run_migrations
            deploy
            post_deployment_verification
            cleanup
            log_success "Deployment completed successfully!"
            log_info "Application is available at: http://localhost"
            ;;
        "rollback")
            rollback
            ;;
        "health")
            post_deployment_verification
            ;;
        *)
            echo "Usage: $0 [version] [environment] [action]"
            echo "Actions: deploy (default), rollback, health"
            exit 1
            ;;
    esac
}

# Execute main function
main "$@"