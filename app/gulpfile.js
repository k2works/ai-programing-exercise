const { src, dest, watch, series, parallel } = require('gulp')
const shell = require('gulp-shell')

// TypeScript のコンパイル
const compile = shell.task(['npx tsc'])

// テスト実行
const test = shell.task(['npm run test'])

// リント実行
const lint = shell.task(['npm run lint'])

// フォーマット実行
const format = shell.task(['npm run format'])

// リント・フォーマット修正
const fix = shell.task(['npm run lint:fix', 'npm run format'])

// ウォッチタスク
function watchTask() {
  watch('src/**/*.ts', series(compile, test))
  watch('test/**/*.ts', test)
}

// ガードタスク
function guardTask() {
  watch('src/**/*.ts', series(compile, lint, test))
  watch('test/**/*.ts', series(lint, test))
}

// チェック・修正タスク
const checkAndFix = series(fix, compile, test)

exports.compile = compile
exports.test = test
exports.lint = lint
exports.format = format
exports.fix = fix
exports.watch = watchTask
exports.guard = guardTask
exports.checkAndFix = checkAndFix
exports.default = checkAndFix