import { defineConfig } from 'vitest/config';

// Root Vitest configuration that aggregates workspace projects.
// This ensures IDEs or root-level invocations (from app/) pick up the
// correct environment per package (e.g., jsdom for frontend).
export default defineConfig({
  projects: [
    // Each entry points to a directory that contains its own vitest.config.ts
    './frontend',
    './backend',
    './shared',
  ],
});
