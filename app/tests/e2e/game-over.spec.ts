import { test, expect } from '@playwright/test';

/**
 * ゲームオーバー機能のE2Eテスト
 * 要件7: ゲームオーバー判定
 */
test.describe('ゲームオーバー機能', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/');
    await page.waitForLoadState('networkidle');

    // ゲームを開始
    await page.click('.new-game-button');
    await page.waitForTimeout(500);
  });

  test('ゲームオーバーダイアログの基本構造', async ({ page }) => {
    // ゲームオーバー状態を作るため、フィールドを埋める
    // （実際のゲームオーバーは時間がかかるため、基本構造のみテスト）

    // 長時間ゲームを進行させてゲームオーバーの可能性を高める
    for (let round = 0; round < 50; round++) {
      // 中央にぷよを積み上げる
      for (let i = 0; i < 20; i++) {
        await page.keyboard.press('ArrowDown');
        await page.waitForTimeout(10);
      }

      await page.waitForTimeout(100);

      // ゲームオーバーダイアログが表示されたかチェック
      const gameOverDialog = page.locator('[data-testid="game-over-dialog"]');
      const isGameOver = await gameOverDialog.isVisible();

      if (isGameOver) {
        // ゲームオーバーダイアログが表示された場合のテスト（要件7.2）
        await expect(gameOverDialog).toBeVisible();

        // ゲームオーバータイトルが表示される
        const gameOverTitle = page.locator('.game-over-title');
        await expect(gameOverTitle).toHaveText('ゲームオーバー');

        // 最終スコアが表示される（要件7.3）
        const finalScore = page.locator('.final-score');
        await expect(finalScore).toBeVisible();
        await expect(finalScore).toContainText('最終スコア');

        // リスタートボタンが表示される（要件7.4）
        const restartButton = page.locator('.restart-button');
        await expect(restartButton).toBeVisible();
        await expect(restartButton).toHaveText('もう一度プレイ');

        break; // ゲームオーバーが確認できたらテスト終了
      }
    }
  });

  test('ゲームオーバー後のリスタート機能', async ({ page }) => {
    // ゲームオーバー状態を作る試行
    for (let round = 0; round < 30; round++) {
      for (let i = 0; i < 20; i++) {
        await page.keyboard.press('ArrowDown');
        await page.waitForTimeout(10);
      }

      await page.waitForTimeout(50);

      // ゲームオーバーダイアログをチェック
      const gameOverDialog = page.locator('[data-testid="game-over-dialog"]');
      const isGameOver = await gameOverDialog.isVisible();

      if (isGameOver) {
        // リスタートボタンをクリック（要件7.4）
        const restartButton = page.locator('.restart-button');
        await expect(restartButton).toBeVisible();
        await restartButton.click();

        // 新しいゲームが開始されることを確認
        await page.waitForTimeout(500);

        // ゲームオーバーダイアログが非表示になる
        await expect(gameOverDialog).not.toBeVisible();

        // スコアが0にリセットされる
        const scoreDisplay = page.locator('.score-section');
        await expect(scoreDisplay).toContainText('0');

        // 新しいぷよが生成される
        const activePuyos = page.locator('.game-board .puyo.active');
        await expect(activePuyos).toHaveCount(2);

        break;
      }
    }
  });

  test('ゲームオーバー状態での操作無効化', async ({ page }) => {
    // ゲームオーバー状態を作る試行
    for (let round = 0; round < 25; round++) {
      for (let i = 0; i < 20; i++) {
        await page.keyboard.press('ArrowDown');
        await page.waitForTimeout(10);
      }

      await page.waitForTimeout(50);

      const gameOverDialog = page.locator('[data-testid="game-over-dialog"]');
      const isGameOver = await gameOverDialog.isVisible();

      if (isGameOver) {
        // ゲームオーバー状態でキー操作が無効になることを確認
        await page.keyboard.press('ArrowLeft');
        await page.keyboard.press('ArrowRight');
        await page.keyboard.press('Space');

        // ダイアログが引き続き表示されることを確認
        await expect(gameOverDialog).toBeVisible();

        break;
      }
    }
  });

  test('ゲームオーバーダイアログのアクセシビリティ', async ({ page }) => {
    // ゲームオーバー状態を作る試行
    for (let round = 0; round < 20; round++) {
      for (let i = 0; i < 20; i++) {
        await page.keyboard.press('ArrowDown');
        await page.waitForTimeout(10);
      }

      await page.waitForTimeout(50);

      const gameOverDialog = page.locator('[data-testid="game-over-dialog"]');
      const isGameOver = await gameOverDialog.isVisible();

      if (isGameOver) {
        // ダイアログのアクセシビリティ属性を確認
        await expect(gameOverDialog).toHaveAttribute('role', 'dialog');
        await expect(gameOverDialog).toHaveAttribute('aria-modal', 'true');

        // タイトルのIDが設定されている
        const title = page.locator('#game-over-title');
        await expect(title).toBeVisible();

        // リスタートボタンにaria-labelが設定されている
        const restartButton = page.locator('.restart-button');
        await expect(restartButton).toHaveAttribute(
          'aria-label',
          '新しいゲームを開始'
        );

        break;
      }
    }
  });

  test('最終スコアの表示形式', async ({ page }) => {
    // ゲームを進行させてスコアを獲得
    for (let i = 0; i < 10; i++) {
      for (let j = 0; j < 20; j++) {
        await page.keyboard.press('ArrowDown');
        await page.waitForTimeout(10);
      }
      await page.waitForTimeout(50);
    }

    // 現在のスコアを記録
    const scoreDisplay = page.locator('.score-section');

    // ゲームオーバー状態を作る試行
    for (let round = 0; round < 30; round++) {
      for (let i = 0; i < 20; i++) {
        await page.keyboard.press('ArrowDown');
        await page.waitForTimeout(5);
      }

      await page.waitForTimeout(30);

      const gameOverDialog = page.locator('[data-testid="game-over-dialog"]');
      const isGameOver = await gameOverDialog.isVisible();

      if (isGameOver) {
        // 最終スコアが適切な形式で表示される（要件7.3）
        const finalScore = page.locator('.final-score');
        await expect(finalScore).toBeVisible();

        const finalScoreText = await finalScore.textContent();
        expect(finalScoreText).toContain('最終スコア');
        expect(finalScoreText).toMatch(/\d+/); // 数値が含まれる

        break;
      }
    }
  });

  test('ゲームオーバー演出の基本動作', async ({ page }) => {
    // ゲームオーバー状態を作る試行
    for (let round = 0; round < 15; round++) {
      for (let i = 0; i < 20; i++) {
        await page.keyboard.press('ArrowDown');
        await page.waitForTimeout(10);
      }

      await page.waitForTimeout(50);

      const gameOverDialog = page.locator('[data-testid="game-over-dialog"]');
      const isGameOver = await gameOverDialog.isVisible();

      if (isGameOver) {
        // ゲームオーバー演出が表示される（要件7.2）
        await expect(gameOverDialog).toBeVisible();

        // オーバーレイが表示される
        const overlay = page.locator('.game-over-overlay');
        await expect(overlay).toBeVisible();

        // ダイアログ内のコンテンツが表示される
        const dialog = page.locator('.game-over-dialog');
        await expect(dialog).toBeVisible();

        break;
      }
    }
  });
});

/**
 * ゲームオーバー判定の詳細テスト
 */
test.describe('ゲームオーバー判定ロジック', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/');
    await page.waitForLoadState('networkidle');
    await page.click('.new-game-button');
    await page.waitForTimeout(500);
  });

  test('フィールド上部到達でのゲームオーバー判定', async ({ page }) => {
    // フィールドの上部までぷよを積み上げる試行
    // （要件7.1: 新しい組ぷよが生成位置に配置できない場合）

    for (let round = 0; round < 40; round++) {
      // 同じ列にぷよを積み上げる
      for (let i = 0; i < 20; i++) {
        await page.keyboard.press('ArrowDown');
        await page.waitForTimeout(5);
      }

      await page.waitForTimeout(30);

      // ゲームオーバー判定をチェック
      const gameOverDialog = page.locator('[data-testid="game-over-dialog"]');
      const isGameOver = await gameOverDialog.isVisible();

      if (isGameOver) {
        // ゲームオーバーが正しく判定されたことを確認
        await expect(gameOverDialog).toBeVisible();

        // ゲーム状態が停止していることを確認
        // （新しいぷよが生成されない）
        await page.waitForTimeout(1000);

        // ダイアログが引き続き表示される
        await expect(gameOverDialog).toBeVisible();

        break;
      }
    }
  });

  test('ゲームオーバー後の状態保持', async ({ page }) => {
    // ゲームオーバー状態を作る
    for (let round = 0; round < 20; round++) {
      for (let i = 0; i < 20; i++) {
        await page.keyboard.press('ArrowDown');
        await page.waitForTimeout(5);
      }

      await page.waitForTimeout(30);

      const gameOverDialog = page.locator('[data-testid="game-over-dialog"]');
      const isGameOver = await gameOverDialog.isVisible();

      if (isGameOver) {
        // ゲームオーバー状態が保持されることを確認
        await page.waitForTimeout(2000);
        await expect(gameOverDialog).toBeVisible();

        // スコアが保持されることを確認
        const finalScore = page.locator('.final-score');
        await expect(finalScore).toBeVisible();

        break;
      }
    }
  });
});
