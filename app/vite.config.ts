/// <reference types="vitest" />
import { defineConfig } from 'vite';
import react from '@vitejs/plugin-react';
import { resolve } from 'path';

// https://vitejs.dev/config/
export default defineConfig({
  plugins: [react()],
  resolve: {
    alias: {
      '@': resolve(__dirname, './src'),
      '@/domain': resolve(__dirname, './src/domain'),
      '@/application': resolve(__dirname, './src/application'),
      '@/infrastructure': resolve(__dirname, './src/infrastructure'),
      '@/presentation': resolve(__dirname, './src/presentation'),
    },
  },
  build: {
    // バンドルサイズ最適化
    minify: 'terser',
    terserOptions: {
      compress: {
        drop_console: true, // console.logを削除
        drop_debugger: true, // debuggerを削除
      },
    },
    rollupOptions: {
      output: {
        // チャンク分割による最適化
        manualChunks: {
          vendor: ['react', 'react-dom'],
          zustand: ['zustand'],
          framer: ['framer-motion'],
        },
      },
    },
    // バンドルサイズ警告の閾値
    chunkSizeWarningLimit: 1000,
  },
  // 開発時のパフォーマンス最適化
  server: {
    hmr: {
      overlay: false, // エラーオーバーレイを無効化してパフォーマンス向上
    },
  },
  test: {
    globals: true,
    environment: 'jsdom',
    setupFiles: ['./src/test/setup.ts'],
    include: ['src/**/*.{test,spec}.{js,mjs,cjs,ts,mts,cts,jsx,tsx}'],
    exclude: [
      '**/node_modules/**',
      '**/dist/**',
      '**/cypress/**',
      '**/.{idea,git,cache,output,temp}/**',
      '**/{karma,rollup,webpack,vite,vitest,jest,ava,babel,nyc,cypress,tsup,build}.config.*',
      'tests/**',
      '**/e2e/**',
    ],
  },
});
