/* eslint-disable no-undef */
import { defineConfig } from 'vitest/config'
import { config } from 'dotenv'

// .env ファイルから環境変数を読み込む
config()

export default defineConfig({
  test: {
    globals: true,
    environment: 'node',
    // テスト実行時に TEST_DATABASE_URL と VITEST を環境変数に設定
    env: {
      VITEST: 'true',
      TEST_DATABASE_URL:
        process.env.TEST_DATABASE_URL ||
        'postgresql://postgres:postgres@localhost:5432/sales_management_test?schema=public'
    },
    coverage: {
      provider: 'v8',
      reporter: ['text', 'html', 'json'],
      reportsDirectory: 'coverage',
      exclude: [
        'dist/**',
        'node_modules/**',
        '**/*.test.ts',
        '**/*.config.js',
        '**/*.config.ts',
        'prisma/**'
      ],
      all: true
    }
  }
})
