import type { NextApiRequest, NextApiResponse } from 'next';
import { ENV } from '@/config/environment';

type HealthStatus = {
  status: 'ok' | 'error';
  timestamp: string;
  environment: string;
  version: string;
  build: {
    id?: string;
    commit?: string;
    branch?: string;
    time?: string;
  };
  checks: {
    api: boolean;
    database?: boolean;
    cache?: boolean;
  };
};

export default function handler(
  req: NextApiRequest,
  res: NextApiResponse<HealthStatus>
) {
  // Only allow GET requests
  if (req.method !== 'GET') {
    res.setHeader('Allow', ['GET']);
    return res.status(405).end(`Method ${req.method} Not Allowed`);
  }

  // Perform health checks
  const apiHealthy = checkAPIHealth();
  const databaseHealthy = checkDatabaseHealth();
  const cacheHealthy = checkCacheHealth();

  const isHealthy =
    apiHealthy && (databaseHealthy ?? true) && (cacheHealthy ?? true);

  const healthStatus: HealthStatus = {
    status: isHealthy ? 'ok' : 'error',
    timestamp: new Date().toISOString(),
    environment: ENV.NODE_ENV,
    version: process.env.npm_package_version || '0.1.0',
    build: {
      id: ENV.BUILD.ID,
      commit: ENV.BUILD.COMMIT,
      branch: ENV.BUILD.BRANCH,
      time: ENV.BUILD.TIME,
    },
    checks: {
      api: apiHealthy,
      database: databaseHealthy,
      cache: cacheHealthy,
    },
  };

  // Set appropriate status code
  const statusCode = isHealthy ? 200 : 503;

  // Set cache headers
  res.setHeader('Cache-Control', 'no-cache, no-store, must-revalidate');
  res.setHeader('X-Content-Type-Options', 'nosniff');

  res.status(statusCode).json(healthStatus);
}

/**
 * Check if the API is responsive
 */
function checkAPIHealth(): boolean {
  // Basic check - if this endpoint is responding, API is healthy
  return true;
}

/**
 * Check database connectivity (when implemented)
 */
function checkDatabaseHealth(): boolean | undefined {
  // TODO: Implement actual database health check when backend is ready
  // For now, return undefined to indicate it's not implemented
  return undefined;
}

/**
 * Check cache service health (when implemented)
 */
function checkCacheHealth(): boolean | undefined {
  // TODO: Implement cache health check when Redis/cache is implemented
  // For now, return undefined to indicate it's not implemented
  return undefined;
}
