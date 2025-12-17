import { defineConfig } from 'vitest/config'

export default defineConfig({
  test: {
    globals: true,
    environment: 'node',
    timeout: 60000, // TestContainerの起動時間を考慮
    hookTimeout: 60000, // フック（beforeAll/afterAllなど）のタイムアウト
    fileParallelism: false, // TestContainerを使うテストは順次実行
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
        'prisma/**',
        'src/test-setup/**',
        'src/generated/**'
      ],
      all: true
    }
  }
})
