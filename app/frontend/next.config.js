/** @type {import('next').NextConfig} */

// Bundle analyzer plugin
const withBundleAnalyzer = require('@next/bundle-analyzer')({
  enabled: process.env.ANALYZE === 'true',
});

const nextConfig = {
  reactStrictMode: true,
  swcMinify: true,

  // Performance optimizations
  experimental: {
    // optimizeCss: true, // Disabled: requires critters package
    // nextScriptWorkers: true, // Disabled: requires @builder.io/partytown
  },

  // Image optimization
  images: {
    domains: ['localhost', 'example.com'],
    formats: ['image/webp', 'image/avif'],
    minimumCacheTTL: 60,
    dangerouslyAllowSVG: false,
    contentSecurityPolicy: "default-src 'self'; script-src 'none'; sandbox;",
  },

  // Webpack configuration
  webpack: (config, { dev, isServer, webpack }) => {
    // Fix for Node.js modules in browser
    if (!isServer) {
      config.resolve.fallback = {
        ...config.resolve.fallback,
        fs: false,
        net: false,
        tls: false,
        crypto: false,
      };
    }

    // Development optimizations
    if (dev) {
      config.watchOptions = {
        poll: 1000,
        aggregateTimeout: 300,
        ignored: ['**/node_modules', '**/.git'],
      };
    }

    // Production optimizations
    if (!dev && !isServer) {
      // Enable tree shaking for ES modules
      config.optimization = {
        ...config.optimization,
        usedExports: true,
        sideEffects: false,
      };

      // Add webpack bundle analyzer in production build
      if (process.env.ANALYZE === 'true') {
        const { BundleAnalyzerPlugin } = require('webpack-bundle-analyzer');
        config.plugins.push(
          new BundleAnalyzerPlugin({
            analyzerMode: 'static',
            reportFilename: '../analyze/client.html',
            openAnalyzer: false,
          })
        );
      }
    }

    // Add build time environment variables
    config.plugins.push(
      new webpack.DefinePlugin({
        'process.env.BUILD_TIME': JSON.stringify(new Date().toISOString()),
        'process.env.BUILD_ID': JSON.stringify(process.env.BUILD_ID || 'local'),
      })
    );

    return config;
  },

  // Compiler options
  compiler: {
    // Remove console logs in production
    removeConsole: process.env.NODE_ENV === 'production' ? {
      exclude: ['error', 'warn'],
    } : false,
    // Enable React DevTools profiling in production
    reactRemoveProperties: process.env.NODE_ENV === 'production' ? {
      properties: ['^data-testid$'],
    } : false,
  },

  // Security headers
  async headers() {
    return [
      {
        source: '/:path*',
        headers: [
          {
            key: 'X-DNS-Prefetch-Control',
            value: 'on',
          },
          {
            key: 'X-Frame-Options',
            value: 'DENY',
          },
          {
            key: 'X-Content-Type-Options',
            value: 'nosniff',
          },
          {
            key: 'X-XSS-Protection',
            value: '1; mode=block',
          },
          {
            key: 'Referrer-Policy',
            value: 'strict-origin-when-cross-origin',
          },
          {
            key: 'Permissions-Policy',
            value: 'camera=(), microphone=(), geolocation=(), interest-cohort=()',
          },
          {
            key: 'Strict-Transport-Security',
            value: 'max-age=63072000; includeSubDomains; preload',
          },
        ],
      },
      // Cache static assets
      {
        source: '/_next/static/:path*',
        headers: [
          {
            key: 'Cache-Control',
            value: 'public, max-age=31536000, immutable',
          },
        ],
      },
      // Cache images
      {
        source: '/images/:path*',
        headers: [
          {
            key: 'Cache-Control',
            value: 'public, max-age=604800, stale-while-revalidate=86400',
          },
        ],
      },
    ];
  },

  // Redirects
  async redirects() {
    return [];
  },

  // Rewrites for API proxy (if needed)
  async rewrites() {
    return [];
  },

  // Output configuration
  output: 'standalone',
  
  // Disable powered by header
  poweredByHeader: false,

  // Enable source maps in production
  productionBrowserSourceMaps: process.env.ENABLE_SOURCE_MAPS === 'true',

  // Trailing slash configuration
  trailingSlash: false,

  // Base path (if deployed to a subdirectory)
  basePath: '',

  // Asset prefix for CDN
  assetPrefix: process.env.ASSET_PREFIX || undefined,

  // Internationalization (future enhancement)
  i18n: undefined,

  // TypeScript configuration
  typescript: {
    // Do not fail build on TypeScript errors in production
    ignoreBuildErrors: process.env.IGNORE_TS_ERRORS === 'true',
  },

  // ESLint configuration
  eslint: {
    // Do not fail build on ESLint errors in production
    ignoreDuringBuilds: process.env.IGNORE_ESLINT === 'true',
    dirs: ['src', 'pages', 'components', 'lib', 'utils'],
  },

  // Environment variables to expose to the browser
  env: {
    BUILD_ENV: process.env.NODE_ENV || 'development',
  },

  // Page extensions (exclude test files)
  pageExtensions: ['tsx', 'ts', 'jsx', 'js'],

  // Compression
  compress: true,

  // Generate build ID
  generateBuildId: async () => {
    // Use git commit hash as build ID if available
    if (process.env.VERCEL_GIT_COMMIT_SHA) {
      return process.env.VERCEL_GIT_COMMIT_SHA.slice(0, 8);
    }
    return `build-${Date.now()}`;
  },

  // Dev indicators
  devIndicators: {
    buildActivity: true,
    buildActivityPosition: 'bottom-right',
  },
};

// Export with bundle analyzer wrapper
module.exports = withBundleAnalyzer(nextConfig);