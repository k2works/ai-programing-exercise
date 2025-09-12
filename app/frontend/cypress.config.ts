import { defineConfig } from 'cypress';

export default defineConfig({
  e2e: {
    // ベースURL
    baseUrl: 'http://localhost:3000',
    
    // テストファイルの場所
    specPattern: 'cypress/e2e/**/*.{cy,spec}.{js,jsx,ts,tsx}',
    
    // サポートファイルの場所  
    supportFile: 'cypress/support/e2e.ts',
    
    // フィクスチャフォルダ
    fixturesFolder: 'cypress/fixtures',
    
    // スクリーンショット
    screenshotsFolder: 'cypress/screenshots',
    
    // ビデオ
    videosFolder: 'cypress/videos',
    
    // ビデオの記録設定
    video: true,
    videoCompression: 32,
    
    // スクリーンショット設定
    screenshotOnRunFailure: true,
    
    // ビューポート設定
    viewportWidth: 1280,
    viewportHeight: 720,
    
    // テスト分離
    testIsolation: true,
    
    // ブラウザ設定
    chromeWebSecurity: false,
    
    // タイムアウト設定
    defaultCommandTimeout: 8000,
    requestTimeout: 10000,
    responseTimeout: 30000,
    pageLoadTimeout: 30000,
    
    // 再試行設定
    retries: {
      runMode: 2,
      openMode: 0,
    },
    
    // 実験的機能
    experimentalStudio: true,
    
    setupNodeEvents(on, config) {
      // ここでNode.jsイベントの設定を行う
      
      // 環境変数の設定
      config.env = {
        ...config.env,
        API_URL: process.env.NEXT_PUBLIC_API_URL || 'http://localhost:5150/api',
        ENABLE_MOCKING: process.env.NEXT_PUBLIC_ENABLE_API_MOCKING || 'true',
      };
      
      // カバレッジの設定（@cypress/code-coverage使用時）
      // require('@cypress/code-coverage/task')(on, config);
      
      // カスタムタスクの追加
      on('task', {
        // データベースのリセット
        resetDB() {
          // データベースリセットロジック
          console.log('Database reset');
          return null;
        },
        
        // ログ出力
        log(message) {
          console.log(message);
          return null;
        },
        
        // 現在時刻の取得
        now() {
          return new Date().toISOString();
        },
      });
      
      // ブラウザ起動オプション
      on('before:browser:launch', (browser, launchOptions) => {
        if (browser.name === 'chrome' && browser.isHeadless) {
          launchOptions.args.push('--disable-gpu');
          launchOptions.args.push('--no-sandbox');
          launchOptions.args.push('--disable-dev-shm-usage');
        }
        
        return launchOptions;
      });
      
      return config;
    },
  },
  
  // コンポーネントテスト設定（将来的に使用）
  component: {
    devServer: {
      framework: 'next',
      bundler: 'webpack',
    },
    specPattern: 'src/**/*.cy.{js,jsx,ts,tsx}',
    supportFile: 'cypress/support/component.ts',
    indexHtmlFile: 'cypress/support/component-index.html',
  },
  
  // 環境変数
  env: {
    // CI/CDで使用される環境変数
    codeCoverage: {
      exclude: [
        'cypress/**/*.*',
        'src/testing/**/*.*',
        'src/**/*.stories.*',
        'src/**/*.d.ts',
      ],
    },
  },
});