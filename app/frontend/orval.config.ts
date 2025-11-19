import { defineConfig } from 'orval';

export default defineConfig({
  api: {
    input: {
      target: 'http://localhost:3000/docs/json',
    },
    output: {
      mode: 'tags-split',
      target: './src/api/generated',
      client: 'axios',
      override: {
        mutator: {
          path: './src/lib/api.ts',
          name: 'apiClient',
        },
      },
    },
  },
});
