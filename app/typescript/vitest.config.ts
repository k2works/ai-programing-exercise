import { defineConfig } from 'vitest/config'

export default defineConfig({
  test: {
    globals: true,
    environment: 'node',
    timeout: 60000, // TestContainerの起動時間を考慮
    coverage: {
      provider: 'v8',
      reporter: ['text', 'json', 'html'],
      reportsDirectory: 'coverage',
      exclude: [
        'dist/**',
        'node_modules/**',
        '**/*.test.ts',
        '**/*.config.js',
        '**/*.config.ts',
        'prisma/**',
        'src/test-setup/**'
      ],
      all: true
    }
  }
})
