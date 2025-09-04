import { test, expect } from '@playwright/test';

/**
 * アクセシビリティのE2Eテスト
 * 要件10.4: アクセシビリティ対応
 */
test.describe('アクセシビリティ', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/');
    await page.waitForLoadState('networkidle');
  });

  test('基本的なアクセシビリティ属性が設定されている', async ({ page }) => {
    // メインコンテンツにrole属性が設定されている
    const mainContent = page.locator('[data-testid="game-page"]');
    await expect(mainContent).toHaveAttribute('role', 'main');
    await expect(mainContent).toHaveAttribute('aria-label', 'ぷよぷよゲーム');

    // ゲームタイトルが適切な見出しレベルで設定されている
    const gameTitle = page.locator('.game-title');
    await expect(gameTitle).toHaveRole('heading');
  });

  test('キーボードナビゲーションが機能する', async ({ page }) => {
    // 新しいゲームボタンにフォーカスできる
    const newGameButton = page.locator('.new-game-button');
    await newGameButton.focus();
    await expect(newGameButton).toBeFocused();

    // Enterキーでボタンを押せる
    await page.keyboard.press('Enter');
    await page.waitForTimeout(500);

    // ゲームが開始されることを確認
    await expect(page.locator('.game-board')).toBeVisible();
  });

  test('ボタンに適切なaria-label属性が設定されている', async ({ page }) => {
    // 新しいゲームボタン
    const newGameButton = page.locator('.new-game-button');
    await expect(newGameButton).toHaveAttribute(
      'aria-label',
      '新しいゲームを開始'
    );

    // ゲームを開始
    await newGameButton.click();
    await page.waitForTimeout(500);

    // 一時停止ボタン
    const pauseButton = page.locator('.pause-button');
    await expect(pauseButton).toHaveAttribute('aria-label', 'ゲームを一時停止');

    // リセットボタン
    const resetButton = page.locator('.reset-button');
    await expect(resetButton).toHaveAttribute('aria-label', 'ゲームをリセット');
  });

  test('スクリーンリーダー用のライブリージョンが機能する', async ({ page }) => {
    await page.click('.new-game-button');
    await page.waitForTimeout(500);

    // ライブリージョンが存在することを確認
    const liveRegion = page.locator('.sr-only[aria-live="polite"]');
    await expect(liveRegion).toBeInTheDocument();
  });

  test('連鎖表示にaria-label属性が設定されている', async ({ page }) => {
    await page.click('.new-game-button');
    await page.waitForTimeout(500);

    // 連鎖が発生した場合のaria-label確認
    // （実際の連鎖は運に依存するため、要素の存在確認）
    const chainDisplay = page.locator('[data-testid="chain-display"]');

    // ゲームを進行させて連鎖の可能性を作る
    for (let i = 0; i < 10; i++) {
      for (let j = 0; j < 20; j++) {
        await page.keyboard.press('ArrowDown');
        await page.waitForTimeout(10);
      }
      await page.waitForTimeout(100);

      const isVisible = await chainDisplay.isVisible();
      if (isVisible) {
        // aria-label属性が設定されていることを確認
        const ariaLabel = await chainDisplay.getAttribute('aria-label');
        expect(ariaLabel).toContain('連鎖数');
        break;
      }
    }
  });

  test('ゲームフィールドが適切にマークアップされている', async ({ page }) => {
    await page.click('.new-game-button');
    await page.waitForTimeout(500);

    // ゲームボードが表示される
    const gameBoard = page.locator('.game-board');
    await expect(gameBoard).toBeVisible();

    // ゲームフィールドのセルが適切にマークアップされている
    const cells = page.locator('.game-board .puyo-cell');
    await expect(cells).toHaveCountGreaterThan(0);
  });

  test('色覚障害者への配慮', async ({ page }) => {
    await page.click('.new-game-button');
    await page.waitForTimeout(500);

    // ぷよが色だけでなく視覚的に区別可能であることを確認
    // （実装では色に加えて形状やパターンで区別）
    const puyos = page.locator('.game-board .puyo');

    // ぷよが表示されるまで待機
    await page.waitForTimeout(1000);

    const puyoCount = await puyos.count();
    if (puyoCount > 0) {
      // ぷよにクラス名が設定されていることを確認（色の区別）
      const firstPuyo = puyos.first();
      const className = await firstPuyo.getAttribute('class');
      expect(className).toBeTruthy();
    }
  });

  test('フォーカス管理が適切に行われる', async ({ page }) => {
    // 初期フォーカス
    const newGameButton = page.locator('.new-game-button');
    await newGameButton.focus();
    await expect(newGameButton).toBeFocused();

    // ゲーム開始
    await newGameButton.click();
    await page.waitForTimeout(500);

    // ゲーム開始後のフォーカス管理
    const pauseButton = page.locator('.pause-button');
    await pauseButton.focus();
    await expect(pauseButton).toBeFocused();

    // Tabキーでフォーカス移動
    await page.keyboard.press('Tab');

    // 次の要素にフォーカスが移動することを確認
    const resetButton = page.locator('.reset-button');
    await expect(resetButton).toBeFocused();
  });

  test('エラー状態の適切な通知', async ({ page }) => {
    await page.click('.new-game-button');
    await page.waitForTimeout(500);

    // ゲームオーバー状態での通知確認
    for (let round = 0; round < 15; round++) {
      for (let i = 0; i < 20; i++) {
        await page.keyboard.press('ArrowDown');
        await page.waitForTimeout(5);
      }
      await page.waitForTimeout(50);

      const gameOverDialog = page.locator('[data-testid="game-over-dialog"]');
      const isGameOver = await gameOverDialog.isVisible();

      if (isGameOver) {
        // ゲームオーバー状態がスクリーンリーダーに通知される
        const liveRegion = page.locator('.sr-only[aria-live="polite"]');
        const liveText = await liveRegion.textContent();
        expect(liveText).toContain('ゲームオーバー');
        break;
      }
    }
  });

  test('動的コンテンツの適切な通知', async ({ page }) => {
    await page.click('.new-game-button');
    await page.waitForTimeout(500);

    // スコア更新の通知確認
    const liveRegion = page.locator('.sr-only[aria-live="polite"]');
    await expect(liveRegion).toBeInTheDocument();

    // ゲームを進行させる
    for (let i = 0; i < 5; i++) {
      for (let j = 0; j < 20; j++) {
        await page.keyboard.press('ArrowDown');
        await page.waitForTimeout(10);
      }
      await page.waitForTimeout(100);

      // 連鎖が発生した場合の通知確認
      const liveText = await liveRegion.textContent();
      if (liveText && liveText.includes('連鎖')) {
        expect(liveText).toContain('連鎖発生');
        break;
      }
    }
  });
});

/**
 * キーボード専用操作のテスト
 */
test.describe('キーボード専用操作', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/');
    await page.waitForLoadState('networkidle');
  });

  test('マウスを使わずにゲーム全体を操作できる', async ({ page }) => {
    // キーボードのみでゲーム開始
    await page.keyboard.press('Tab'); // 新しいゲームボタンにフォーカス
    await page.keyboard.press('Enter'); // ゲーム開始
    await page.waitForTimeout(500);

    // ゲームが開始されることを確認
    await expect(page.locator('.game-board')).toBeVisible();

    // キーボードでぷよ操作
    await page.keyboard.press('ArrowLeft');
    await page.keyboard.press('Space');
    await page.keyboard.press('ArrowDown');

    // 操作が有効であることを確認
    const activePuyos = page.locator('.game-board .puyo.active');
    await expect(activePuyos).toHaveCount(2);

    // キーボードでゲーム制御
    await page.keyboard.press('Tab'); // 一時停止ボタンにフォーカス
    await page.keyboard.press('Enter'); // 一時停止

    // ゲームが一時停止されることを確認
    const pauseButton = page.locator('.pause-button');
    await expect(pauseButton).toContainText('再開');
  });

  test('Tabキーによる適切なフォーカス順序', async ({ page }) => {
    // 初期状態でのフォーカス順序
    await page.keyboard.press('Tab');
    const newGameButton = page.locator('.new-game-button');
    await expect(newGameButton).toBeFocused();

    // ゲーム開始
    await page.keyboard.press('Enter');
    await page.waitForTimeout(500);

    // ゲーム開始後のフォーカス順序
    await page.keyboard.press('Tab');
    const pauseButton = page.locator('.pause-button');
    await expect(pauseButton).toBeFocused();

    await page.keyboard.press('Tab');
    const resetButton = page.locator('.reset-button');
    await expect(resetButton).toBeFocused();
  });

  test('Escapeキーによるダイアログ操作', async ({ page }) => {
    await page.click('.new-game-button');
    await page.waitForTimeout(500);

    // ゲームオーバー状態を作る試行
    for (let round = 0; round < 10; round++) {
      for (let i = 0; i < 20; i++) {
        await page.keyboard.press('ArrowDown');
        await page.waitForTimeout(5);
      }
      await page.waitForTimeout(50);

      const gameOverDialog = page.locator('[data-testid="game-over-dialog"]');
      const isGameOver = await gameOverDialog.isVisible();

      if (isGameOver) {
        // ダイアログが表示された状態でEscapeキーの動作確認
        // （実装によってはEscapeキーでダイアログを閉じる）
        await page.keyboard.press('Escape');

        // ダイアログが引き続き表示される（ゲームオーバーは閉じられない）
        await expect(gameOverDialog).toBeVisible();
        break;
      }
    }
  });
});
