/**
 * Jest テストセットアップファイル
 * 全てのテストファイルで共通して使用される設定
 */

// カスタムマッチャーの追加
expect.extend({
  /**
   * 数値が偶数かどうかをテストするカスタムマッチャー
   * @param {number} received テスト対象の値
   * @returns {object} マッチャーの結果
   */
  toBeEven(received) {
    const pass = received % 2 === 0;
    if (pass) {
      return {
        message: () => `期待値: ${received} は偶数であってはいけません`,
        pass: true,
      };
    } else {
      return {
        message: () => `期待値: ${received} は偶数であるべきです`,
        pass: false,
      };
    }
  },

  /**
   * 数値が奇数かどうかをテストするカスタムマッチャー
   * @param {number} received テスト対象の値
   * @returns {object} マッチャーの結果
   */
  toBeOdd(received) {
    const pass = received % 2 === 1;
    if (pass) {
      return {
        message: () => `期待値: ${received} は奇数であってはいけません`,
        pass: true,
      };
    } else {
      return {
        message: () => `期待値: ${received} は奇数であるべきです`,
        pass: false,
      };
    }
  }
});

// グローバルなテスト前処理
beforeAll(() => {
  console.log('🚀 テストスイートを開始します');
});

// グローバルなテスト後処理
afterAll(() => {
  console.log('✅ 全てのテストが完了しました');
});

// 各テストケース前の処理
beforeEach(() => {
  // モックのリセットなど必要に応じて追加
});

// 各テストケース後の処理
afterEach(() => {
  // クリーンアップ処理など必要に応じて追加
});

// コンソールログのモック（必要に応じて）
const originalConsoleLog = console.log;
console.log = (...args) => {
  // テスト実行時にコンソールログを制御したい場合
  if (process.env.NODE_ENV === 'test' && process.env.SILENT_TESTS === 'true') {
    return;
  }
  originalConsoleLog.apply(console, args);
};

// エラーハンドリングの強化
process.on('unhandledRejection', (reason, promise) => {
  console.error('未処理のPromise拒否:', promise, '理由:', reason);
});

// テスト環境での時間の固定（必要に応じて）
const mockDate = new Date('2025-07-02T00:00:00.000Z');
global.Date = class extends Date {
  constructor(...args) {
    if (args.length === 0) {
      return mockDate;
    }
    return new (Function.prototype.bind.apply(Date, [null].concat(args)));
  }
  
  static now() {
    return mockDate.getTime();
  }
};
