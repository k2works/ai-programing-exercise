const nextJest = require('next/jest');

const createJestConfig = nextJest({
  dir: './',
});

const customJestConfig = {
  // テスト実行後のセットアップファイル
  setupFilesAfterEnv: [
    '<rootDir>/src/testing/setup-tests-simple.ts',
  ],
  
  // テスト実行前のセットアップファイル（環境変数など）
  setupFiles: ['<rootDir>/src/testing/setup-env.ts'],
  
  // モジュール検索ディレクトリ
  moduleDirectories: ['node_modules', '<rootDir>/'],
  
  // テスト環境
  testEnvironment: 'jest-environment-jsdom',
  
  // モジュール名のマッピング（パス エイリアス）
  moduleNameMapping: {
    '@/(.*)': '<rootDir>/src/$1',
    '\\.(css|less|scss|sass)$': 'identity-obj-proxy',
  },
  
  // 変換を無視するパッケージの設定
  transformIgnorePatterns: [
    'node_modules/(?!(react-error-boundary|@tanstack|zustand)/)',
  ],
  
  // 無視するテストパス
  testPathIgnorePatterns: [
    '<rootDir>/cypress/',
    '<rootDir>/code-stages/',
    '<rootDir>/.next/',
    '<rootDir>/node_modules/',
  ],
  
  // カバレッジ収集対象ファイル
  collectCoverageFrom: [
    'src/**/*.{js,jsx,ts,tsx}',
    '!src/**/*.d.ts',
    '!src/**/*.stories.{js,jsx,ts,tsx}',
    '!src/testing/**',
    '!src/pages/_app.tsx',
    '!src/pages/_document.tsx',
    '!src/pages/api/**/*',
  ],
  
  // カバレッジ閾値
  coverageThreshold: {
    global: {
      branches: 80,
      functions: 80,
      lines: 80,
      statements: 80,
    },
  },
  
  // カバレッジレポートの出力形式
  coverageReporters: [
    'text',
    'lcov',
    'html',
    'json-summary',
  ],
  
  // テストタイムアウト
  testTimeout: 10000,
  
  // 詳細出力
  verbose: true,
};

module.exports = createJestConfig(customJestConfig);