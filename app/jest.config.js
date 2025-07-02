/**
 * Jest設定ファイル
 * テスト実行環境の詳細設定
 */

export default {
  // テスト環境の設定
  testEnvironment: 'node',
  
  // モジュール変換の設定
  transform: {
    '^.+\\.js$': 'babel-jest'
  },
  
  // テストファイルのパターン
  testMatch: [
    '**/test/**/*.test.js',
    '**/__tests__/**/*.js',
    '**/?(*.)+(spec|test).js'
  ],
  
  // カバレッジ収集の対象ファイル
  collectCoverageFrom: [
    'src/**/*.js',
    '!src/index.js',
    '!src/tdd-guide.js'
  ],
  
  // カバレッジレポートの出力ディレクトリ
  coverageDirectory: 'coverage',
  
  // カバレッジレポートの形式
  coverageReporters: [
    'text',
    'text-summary',
    'lcov',
    'html',
    'json'
  ],
  
  // カバレッジの閾値設定
  coverageThreshold: {
    global: {
      branches: 80,
      functions: 80,
      lines: 80,
      statements: 80
    }
  },
  
  // テスト実行時の詳細表示
  verbose: true,
  
  // テスト結果の色付け
  colors: true,
  
  // テスト実行前後のセットアップファイル
  setupFilesAfterEnv: ['<rootDir>/test/setup.js'],
  
  // モジュール解決のパス設定
  moduleDirectories: ['node_modules', 'src'],
  
  // ファイル拡張子の解決順序
  moduleFileExtensions: ['js', 'json'],
  
  // テスト結果の出力形式
  reporters: [
    'default',
    ['jest-html-reporter', {
      'pageTitle': 'テスト結果レポート',
      'outputPath': 'coverage/test-report.html',
      'includeFailureMsg': true,
      'includeSuiteFailure': true
    }]
  ],
  
  // テスト実行時のタイムアウト設定
  testTimeout: 10000,
  
  // ウォッチモードの設定
  watchPlugins: [
    'jest-watch-typeahead/filename',
    'jest-watch-typeahead/testname'
  ]
};
