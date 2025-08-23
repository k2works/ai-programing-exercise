import { defineConfig, devices } from '@playwright/test';

// Vitestのグローバル設定をクリア
if (typeof globalThis !== 'undefined') {
  // @ts-ignore
  delete globalThis.expect;
  // @ts-ignore  
  delete globalThis.test;
  // @ts-ignore
  delete globalThis.describe;
  // @ts-ignore
  delete globalThis.it;
}

/**
 * @see https://playwright.dev/docs/test-configuration
 */
export default defineConfig({
  testDir: './tests/e2e',
  fullyParallel: true,
  forbidOnly: !!process.env.CI,
  retries: process.env.CI ? 2 : 0,
  workers: process.env.CI ? 1 : undefined,
  reporter: 'html',
  
  use: {
    baseURL: 'http://localhost:4173',
    trace: 'on-first-retry',
  },

  projects: [
    {
      name: 'chromium',
      use: { ...devices['Desktop Chrome'] },
    },
  ],

  webServer: {
    command: 'npm run preview',
    url: 'http://localhost:4173',
    reuseExistingServer: !process.env.CI,
  },
});