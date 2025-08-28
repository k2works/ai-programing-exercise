import { defineConfig } from 'vite'

// https://vitejs.dev/config/
export default defineConfig({
  root: 'src',
  build: {
    outDir: '../dist',
    emptyOutDir: true,
    assetsDir: 'assets',
  },
  server: {
    port: 3000,
    open: true,
  },
  assetsInclude: [
    '**/*.png',
    '**/*.jpg',
    '**/*.jpeg',
    '**/*.gif',
    '**/*.svg',
    '**/*.mp3',
    '**/*.wav',
    '**/*.ogg',
  ],
  optimizeDeps: {
    include: ['phaser'],
  },
})
