# MRS Production Deployment Guide

## Prerequisites

### System Requirements
- Linux server (Ubuntu 20.04+ recommended)
- Docker 24.0+ and Docker Compose 2.0+
- Minimum 4GB RAM, 2 CPU cores
- 50GB+ disk space
- SSL certificate for HTTPS

### Dependencies Installation

```bash
# Install Docker
curl -fsSL https://get.docker.com -o get-docker.sh
sudo sh get-docker.sh
sudo usermod -aG docker $USER

# Install Docker Compose
sudo curl -L "https://github.com/docker/compose/releases/latest/download/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose
sudo chmod +x /usr/local/bin/docker-compose
```

## Quick Start

### 1. Environment Configuration

```bash
# Copy environment template
cp .env.production.example .env.production

# Edit configuration with your values
nano .env.production
```

**Required Configuration:**
- `DATABASE_CONNECTION_STRING`: PostgreSQL connection
- `JWT_SECRET_KEY`: 256-bit secret for JWT tokens
- `ALLOWED_ORIGINS`: Frontend domain URLs
- `SMTP_*`: Email server configuration

### 2. SSL Certificate Setup

```bash
# For Let's Encrypt certificates
sudo apt install certbot
sudo certbot certonly --standalone -d yourdomain.com -d www.yourdomain.com

# Copy certificates to expected locations
sudo cp /etc/letsencrypt/live/yourdomain.com/fullchain.pem /etc/ssl/certs/
sudo cp /etc/letsencrypt/live/yourdomain.com/privkey.pem /etc/ssl/private/
```

### 3. Deploy Application

```bash
# Make deployment script executable
chmod +x deployment/scripts/deploy.sh

# Run deployment
./deployment/scripts/deploy.sh v1.0.0 production
```

## Deployment Process

### Automated Deployment

The deployment script performs:

1. **Pre-deployment Checks**
   - Docker service verification
   - Environment file validation
   - Directory structure setup

2. **Image Building**
   - Backend (.NET API)
   - Frontend (React + Nginx)

3. **Zero-Downtime Deployment**
   - Blue-Green deployment strategy
   - Health check validation
   - Automatic rollback on failure

4. **Post-deployment Verification**
   - Service health checks
   - API endpoint testing
   - Database connectivity

### Manual Deployment Steps

```bash
# 1. Build images
cd deployment/production
docker-compose -f docker-compose.prod.yml build

# 2. Start services
docker-compose -f docker-compose.prod.yml up -d

# 3. Check status
docker-compose -f docker-compose.prod.yml ps
```

## Service Management

### Start Services
```bash
cd deployment/production
docker-compose -f docker-compose.prod.yml up -d
```

### Stop Services
```bash
docker-compose -f docker-compose.prod.yml down
```

### View Logs
```bash
# All services
docker-compose -f docker-compose.prod.yml logs -f

# Specific service
docker-compose -f docker-compose.prod.yml logs -f mrs-backend
```

### Restart Service
```bash
docker-compose -f docker-compose.prod.yml restart mrs-backend
```

## Monitoring & Maintenance

### Health Checks

- **Application**: `http://localhost:8080/health`
- **Frontend**: `http://localhost/health`
- **Database**: Automatic via Docker health checks
- **Prometheus**: `http://localhost:9090`
- **Grafana**: `http://localhost:3000` (admin/password from env)

### Log Management

Logs are stored in:
- Application: `/var/log/mrs/`
- Nginx: `/var/log/nginx/`
- Database: Docker logs

### Backup Management

```bash
# Manual backup
docker exec mrs-postgres-prod pg_dump -U mrs_user mrs_production > backup_$(date +%Y%m%d_%H%M%S).sql

# Automated backups run daily via the application
```

### Updates

```bash
# Deploy new version
./deployment/scripts/deploy.sh v1.1.0 production

# Rollback to previous version
./deployment/scripts/deploy.sh v1.0.0 production rollback
```

## Security Considerations

### SSL/TLS Configuration
- Use strong SSL certificates (Let's Encrypt recommended)
- Enable HSTS headers
- Configure secure cipher suites

### Database Security
- Use strong passwords
- Enable connection encryption
- Regular security updates

### Network Security
- Configure firewall (UFW recommended)
- Use private networks for inter-service communication
- Regular security scanning

### Application Security
- JWT token rotation
- Rate limiting enabled
- Input validation
- Security headers configured

## Troubleshooting

### Common Issues

**1. Container fails to start**
```bash
# Check logs
docker-compose -f docker-compose.prod.yml logs service-name

# Check resource usage
docker stats
```

**2. Database connection issues**
```bash
# Test connection
docker exec -it mrs-postgres-prod psql -U mrs_user -d mrs_production

# Check network connectivity
docker network ls
docker network inspect mrs_network
```

**3. SSL certificate issues**
```bash
# Verify certificate
openssl x509 -in /etc/ssl/certs/fullchain.pem -text -noout

# Renew Let's Encrypt certificate
sudo certbot renew
```

### Performance Monitoring

Access Grafana dashboard:
1. Navigate to `http://localhost:3000`
2. Login with admin credentials from environment
3. Import MRS dashboard from `monitoring/grafana/dashboards/`

### Support

For technical support or deployment issues:
1. Check application logs: `/var/log/mrs/`
2. Review monitoring dashboards
3. Consult system health endpoints
4. Create issue in project repository

## Disaster Recovery

### Backup Strategy
- Daily automated database backups
- Application data backup
- Configuration backup
- Weekly full system backup

### Recovery Procedures
1. Restore from latest backup
2. Verify data integrity
3. Test all critical functions
4. Update DNS if needed
5. Monitor system health

### High Availability Setup
For production environments requiring high availability:
- Multi-instance deployment
- Load balancer configuration
- Database replication
- Automated failover