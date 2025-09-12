/** @type {import('next').NextConfig} */

const nextConfig = {
  // 実験的機能の有効化
  experimental: {
    // CSS最適化
    optimizeCss: true,
  },

  // 環境変数の設定
  env: {
    CUSTOM_KEY: process.env.CUSTOM_KEY,
  },

  // 公開する環境変数 (NEXT_PUBLIC_ プレフィックス不要な場合)
  publicRuntimeConfig: {
    API_URL: process.env.NEXT_PUBLIC_API_URL,
    ENABLE_MOCKING: process.env.NEXT_PUBLIC_ENABLE_API_MOCKING,
  },

  // Webpack設定のカスタマイズ
  webpack: (config, { buildId, dev, isServer, defaultLoaders, webpack }) => {
    // サーバーサイド以外でのNode.js modulesの fallback
    if (!isServer) {
      config.resolve.fallback = {
        ...config.resolve.fallback,
        fs: false,
        net: false,
        tls: false,
        crypto: false,
      };
    }

    // Bundle analyzer (ANALYZE=true で有効)
    if (process.env.ANALYZE === 'true') {
      const withBundleAnalyzer = require('@next/bundle-analyzer')({
        enabled: true,
      });
      return withBundleAnalyzer.webpack(config, {
        buildId,
        dev,
        isServer,
        defaultLoaders,
        webpack,
      });
    }

    // TypeScriptの設定
    config.resolve.alias = {
      ...config.resolve.alias,
      '@': require('path').resolve(__dirname, 'src'),
    };

    return config;
  },

  // 画像最適化設定
  images: {
    // 外部画像ドメインの許可
    domains: [
      'example.com',
      'via.placeholder.com',
      'images.unsplash.com',
    ],
    // 画像フォーマットの最適化
    formats: ['image/webp', 'image/avif'],
    // 画像サイズの設定
    deviceSizes: [640, 750, 828, 1080, 1200, 1920, 2048, 3840],
    imageSizes: [16, 32, 48, 64, 96, 128, 256, 384],
  },

  // セキュリティヘッダーの設定
  async headers() {
    return [
      {
        source: '/(.*)',
        headers: [
          {
            key: 'X-Frame-Options',
            value: 'DENY',
          },
          {
            key: 'X-Content-Type-Options',
            value: 'nosniff',
          },
          {
            key: 'Referrer-Policy',
            value: 'origin-when-cross-origin',
          },
          {
            key: 'X-XSS-Protection',
            value: '1; mode=block',
          },
          {
            key: 'Permissions-Policy',
            value: 'camera=(), microphone=(), geolocation=()',
          },
        ],
      },
    ];
  },

  // リダイレクト設定
  async redirects() {
    return [
      {
        source: '/home',
        destination: '/',
        permanent: true,
      },
      {
        source: '/jobs',
        destination: '/dashboard/jobs',
        permanent: false,
        has: [
          {
            type: 'cookie',
            key: 'authenticated',
            value: 'true',
          },
        ],
      },
    ];
  },

  // リライト設定 (API プロキシなど)
  async rewrites() {
    return [
      {
        source: '/api/proxy/:path*',
        destination: `${process.env.NEXT_PUBLIC_API_URL || 'http://localhost:8080'}/:path*`,
      },
    ];
  },

  // 静的最適化の設定
  trailingSlash: false,
  
  // 出力設定
  output: 'standalone', // Docker環境での最適化
  
  // PoweredBy ヘッダーを無効化
  poweredByHeader: false,

  // 圧縮設定
  compress: true,

  // SWC minifier (Terserより高速)
  swcMinify: true,

  // React Strict Mode
  reactStrictMode: true,

  // ESLint設定
  eslint: {
    // ビルド時にESLintを実行
    ignoreDuringBuilds: false,
    dirs: ['src'], // ESLintを実行するディレクトリ
  },

  // TypeScript設定
  typescript: {
    // 型エラーがある場合でもビルドを続行するか
    ignoreBuildErrors: false,
  },

  // 国際化設定 (将来の拡張用)
  // i18n: {
  //   locales: ['en', 'ja'],
  //   defaultLocale: 'ja',
  // },
};

module.exports = nextConfig;