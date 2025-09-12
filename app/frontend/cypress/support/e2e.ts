// Cypress E2E サポートファイル
// このファイルは全てのテストファイルより前に自動的に読み込まれます

import './commands';

// カスタムコマンドの型定義をインポート
/// <reference types="./commands" />

// セットアップ設定
beforeEach(() => {
  // 各テスト前に実行される設定
  
  // ローカルストレージとセッションストレージをクリア
  cy.clearLocalStorage();
  cy.clearCookies();
  
  // API モッキングの設定
  if (Cypress.env('ENABLE_MOCKING') === 'true') {
    // MSWの開始（必要に応じて）
    cy.window().then((win) => {
      // MSWがグローバルに利用可能な場合
      if (win.msw) {
        win.msw.worker.start({
          onUnhandledRequest: 'bypass',
        });
      }
    });
  }
});

// グローバル設定
Cypress.on('uncaught:exception', (err, runnable) => {
  // 特定のエラーを無視する場合
  if (err.message.includes('ResizeObserver loop limit exceeded')) {
    return false;
  }
  
  // Hydration エラーを無視（開発環境でよく発生）
  if (err.message.includes('Hydration')) {
    return false;
  }
  
  // その他のエラーはテストを失敗させる
  return true;
});

// ビューポートのプリセット
const viewports = {
  mobile: [375, 667],
  tablet: [768, 1024], 
  desktop: [1280, 720],
  large: [1920, 1080],
};

// カスタム設定をグローバルに追加
declare global {
  namespace Cypress {
    interface Chainable {
      setViewport(preset: keyof typeof viewports): Chainable<void>;
    }
  }
}

// ビューポート設定のカスタムコマンド
Cypress.Commands.add('setViewport', (preset: keyof typeof viewports) => {
  const [width, height] = viewports[preset];
  cy.viewport(width, height);
});

// アクセシビリティテスト用の設定（cypress-axe使用時）
// import 'cypress-axe';

// パフォーマンステスト用の設定
// import 'cypress-lighthouse';

// ビジュアルリグレッションテスト用の設定（cypress-image-snapshot使用時）
// import 'cypress-image-snapshot/command';

export {};