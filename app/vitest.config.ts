/// <reference types="vitest" />
import { defineConfig } from 'vite'

export default defineConfig({
  test: {
    globals: true,
    environment: 'jsdom',
    coverage: {
      provider: 'c8',
      reporter: ['html', 'text', 'lcov'],
      reportsDirectory: './coverage',
    },
  },
})
