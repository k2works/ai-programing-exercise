import { defineConfig } from 'vitest/config'

export default defineConfig({
  test: {
    globals: true,
    environment: 'jsdom',
    setupFiles: ['src/test/setup.ts'],
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
        'vite.config.ts',
        'vitest.config.ts',
        'gulpfile.js',
        'src/main.ts',
        'src/counter.ts',
        'src/vite-env.d.ts',
      ],
      all: true,
    },
  },
})
