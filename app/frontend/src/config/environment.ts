/**
 * Environment Configuration
 * Centralized configuration for all environment variables
 */

export const ENV = {
  // Application
  APP_NAME: process.env.NEXT_PUBLIC_APP_NAME || 'Jobs Board',
  APP_URL: process.env.NEXT_PUBLIC_APP_URL || 'http://localhost:3000',
  NODE_ENV: process.env.NODE_ENV || 'development',
  IS_PRODUCTION: process.env.NODE_ENV === 'production',
  IS_DEVELOPMENT: process.env.NODE_ENV === 'development',
  IS_TEST: process.env.NODE_ENV === 'test',

  // API Configuration
  API: {
    BASE_URL: process.env.NEXT_PUBLIC_API_URL || 'http://localhost:3000/api',
    TIMEOUT: parseInt(process.env.NEXT_PUBLIC_API_TIMEOUT || '30000', 10),
    ENABLE_MOCKING: process.env.NEXT_PUBLIC_ENABLE_API_MOCKING === 'true',
    MSW_DELAY: parseInt(process.env.NEXT_PUBLIC_MSW_DELAY || '0', 10),
  },

  // Authentication
  AUTH: {
    ENABLED: process.env.NEXT_PUBLIC_AUTH_ENABLED === 'true',
    NEXTAUTH_URL: process.env.NEXTAUTH_URL || 'http://localhost:3000',
    // Note: NEXTAUTH_SECRET should only be accessed server-side
  },

  // Feature Flags
  FEATURES: {
    ANALYTICS: process.env.NEXT_PUBLIC_ENABLE_ANALYTICS === 'true',
    NOTIFICATIONS: process.env.NEXT_PUBLIC_ENABLE_NOTIFICATIONS === 'true',
    DEBUG_MODE: process.env.NEXT_PUBLIC_ENABLE_DEBUG_MODE === 'true',
    PERFORMANCE_MONITORING:
      process.env.NEXT_PUBLIC_ENABLE_PERFORMANCE_MONITORING === 'true',
  },

  // Third-party Services
  SERVICES: {
    GA_TRACKING_ID: process.env.NEXT_PUBLIC_GA_TRACKING_ID,
    PLAUSIBLE_DOMAIN: process.env.NEXT_PUBLIC_PLAUSIBLE_DOMAIN,
    SENTRY_DSN: process.env.NEXT_PUBLIC_SENTRY_DSN,
  },

  // Development
  DEV: {
    ANALYZE: process.env.ANALYZE === 'true',
    LOG_LEVEL: process.env.NEXT_PUBLIC_LOG_LEVEL || 'info',
  },

  // Build Information
  BUILD: {
    ID: process.env.BUILD_ID,
    COMMIT: process.env.NEXT_PUBLIC_GIT_COMMIT,
    BRANCH: process.env.NEXT_PUBLIC_GIT_BRANCH,
    TIME: process.env.NEXT_PUBLIC_BUILD_TIME,
  },

  // Security
  SECURITY: {
    CSP_ENABLED: process.env.NEXT_PUBLIC_CSP_ENABLED !== 'false',
    RATE_LIMIT: {
      ENABLED: process.env.NEXT_PUBLIC_RATE_LIMIT_ENABLED === 'true',
      MAX_REQUESTS: parseInt(
        process.env.NEXT_PUBLIC_RATE_LIMIT_MAX_REQUESTS || '100',
        10
      ),
      WINDOW_MS: parseInt(
        process.env.NEXT_PUBLIC_RATE_LIMIT_WINDOW_MS || '60000',
        10
      ),
    },
  },
} as const;

/**
 * Type-safe environment validation
 */
export function validateEnvironment(): void {
  const requiredVars = ['NEXT_PUBLIC_APP_NAME', 'NEXT_PUBLIC_API_URL'];

  const missingVars = requiredVars.filter((varName) => !process.env[varName]);

  if (missingVars.length > 0 && ENV.IS_PRODUCTION) {
    throw new Error(
      `Missing required environment variables: ${missingVars.join(', ')}`
    );
  }

  // Validate URLs
  try {
    new URL(ENV.API.BASE_URL);
  } catch {
    console.warn(`Invalid API URL: ${ENV.API.BASE_URL}`);
  }
}

/**
 * Get environment-specific configuration
 */
export function getEnvironmentConfig() {
  return (
    {
      development: {
        apiMocking: true,
        debugMode: true,
        strictMode: true,
      },
      test: {
        apiMocking: true,
        debugMode: false,
        strictMode: false,
      },
      production: {
        apiMocking: false,
        debugMode: false,
        strictMode: false,
      },
    }[ENV.NODE_ENV] || {
      apiMocking: false,
      debugMode: false,
      strictMode: false,
    }
  );
}

/**
 * Feature flag checker
 */
export function isFeatureEnabled(feature: keyof typeof ENV.FEATURES): boolean {
  return ENV.FEATURES[feature] === true;
}

/**
 * Environment logger
 */
export function logEnvironment(): void {
  if (ENV.IS_DEVELOPMENT) {
    console.group('🔧 Environment Configuration');
    console.log('NODE_ENV:', ENV.NODE_ENV);
    console.log('API URL:', ENV.API.BASE_URL);
    console.log('Features:', ENV.FEATURES);
    console.log('Build:', ENV.BUILD);
    console.groupEnd();
  }
}
