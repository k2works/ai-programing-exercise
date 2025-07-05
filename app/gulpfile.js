import { watch, series } from 'gulp'
import shell from 'gulp-shell'

// テストタスク
export const test = shell.task(['npm run test'])

// テストカバレッジタスク
export const coverage = shell.task(['npm run test:coverage'])

// 静的コード解析タスク
export const lint = shell.task(['npx eslint . --ext .ts,.tsx'])

// 自動修正付き静的コード解析タスク
export const lintFix = shell.task(['npx eslint . --ext .ts,.tsx --fix'])

// フォーマットタスク
export const format = shell.task(['npx prettier --write .'])

// フォーマットチェックタスク
export const formatCheck = shell.task(['npx prettier --check .'])

// ビルドタスク
export const build = shell.task(['npm run build'])

// 開発サーバータスク
export const dev = shell.task(['npm run dev'])

// 全体チェックタスク（自動修正付き）
export const checkAndFix = series(lintFix, format, test)

// ファイル監視タスク（Ruby入門2のGuardに対応）
export function guard() {
  console.log('🔍 Guard is watching for file changes...')
  console.log(
    'Files will be automatically linted, formatted, and tested on change.'
  )
  watch('src/**/*.ts', series(lintFix, format, test))
  watch('**/*.test.ts', series(test))
}

// ファイル監視タスク
export function watchFiles() {
  watch('src/**/*.ts', series(formatCheck, lint, test))
  watch('**/*.test.ts', series(test))
}

// デフォルトタスク（Ruby入門2のGuardのような自動化）
export default series(checkAndFix, guard)

// ウォッチタスクのエイリアス
export { watchFiles as watch }
