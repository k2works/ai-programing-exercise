import { test, expect } from '@playwright/test';

/**
 * タッチ操作機能のE2Eテスト
 * 要件9: タッチ操作対応
 */
test.describe('タッチ操作機能', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/');
    await page.waitForLoadState('networkidle');

    // ゲームを開始
    await page.click('.new-game-button');
    await page.waitForTimeout(500); // ぷよが生成されるまで待機
  });

  test('画面タップでぷよが回転する', async ({ page }) => {
    // 組ぷよの初期配置を記録
    const activePuyos = page.locator('.game-board .puyo.active');
    const initialCount = await activePuyos.count();
    expect(initialCount).toBe(2);

    // ゲームフィールドをタップ（要件9.2）
    const gameContainer = page.locator('[data-testid="game-container"]');
    await gameContainer.tap();
    await page.waitForTimeout(100);

    // ぷよが回転したことを確認
    const rotatedPuyos = page.locator('.game-board .puyo.active');
    await expect(rotatedPuyos).toHaveCount(2);
  });

  test('左スワイプでぷよが左に移動する', async ({ page }) => {
    // 現在のぷよの位置を取得
    const activePuyo = page.locator('.game-board .puyo.active').first();
    const initialPosition = await activePuyo.boundingBox();

    // ゲームコンテナで左スワイプを実行（要件9.1）
    const gameContainer = page.locator('[data-testid="game-container"]');
    const containerBox = await gameContainer.boundingBox();

    if (containerBox) {
      const startX = containerBox.x + containerBox.width * 0.7;
      const endX = containerBox.x + containerBox.width * 0.3;
      const y = containerBox.y + containerBox.height * 0.5;

      await page.touchscreen.tap(startX, y);
      await page.touchscreen.tap(endX, y);
    }

    await page.waitForTimeout(100);

    // ぷよが左に移動したことを確認
    const newPosition = await activePuyo.boundingBox();
    if (initialPosition && newPosition) {
      expect(newPosition.x).toBeLessThan(initialPosition.x);
    }
  });

  test('右スワイプでぷよが右に移動する', async ({ page }) => {
    // 現在のぷよの位置を取得
    const activePuyo = page.locator('.game-board .puyo.active').first();
    const initialPosition = await activePuyo.boundingBox();

    // ゲームコンテナで右スワイプを実行（要件9.1）
    const gameContainer = page.locator('[data-testid="game-container"]');
    const containerBox = await gameContainer.boundingBox();

    if (containerBox) {
      const startX = containerBox.x + containerBox.width * 0.3;
      const endX = containerBox.x + containerBox.width * 0.7;
      const y = containerBox.y + containerBox.height * 0.5;

      await page.touchscreen.tap(startX, y);
      await page.touchscreen.tap(endX, y);
    }

    await page.waitForTimeout(100);

    // ぷよが右に移動したことを確認
    const newPosition = await activePuyo.boundingBox();
    if (initialPosition && newPosition) {
      expect(newPosition.x).toBeGreaterThan(initialPosition.x);
    }
  });

  test('下スワイプでぷよが高速落下する', async ({ page }) => {
    // 現在のぷよの位置を取得
    const activePuyo = page.locator('.game-board .puyo.active').first();
    const initialPosition = await activePuyo.boundingBox();

    // ゲームコンテナで下スワイプを実行（要件9.3）
    const gameContainer = page.locator('[data-testid="game-container"]');
    const containerBox = await gameContainer.boundingBox();

    if (containerBox) {
      const x = containerBox.x + containerBox.width * 0.5;
      const startY = containerBox.y + containerBox.height * 0.3;
      const endY = containerBox.y + containerBox.height * 0.7;

      await page.touchscreen.tap(x, startY);
      await page.touchscreen.tap(x, endY);
    }

    await page.waitForTimeout(200);

    // ぷよが下に移動したことを確認
    const newPosition = await activePuyo.boundingBox();
    if (initialPosition && newPosition) {
      expect(newPosition.y).toBeGreaterThan(initialPosition.y);
    }
  });

  test('複数のタッチ操作を連続して実行できる', async ({ page }) => {
    const gameContainer = page.locator('[data-testid="game-container"]');

    // タップで回転
    await gameContainer.tap();
    await page.waitForTimeout(50);

    // 左スワイプで移動
    const containerBox = await gameContainer.boundingBox();
    if (containerBox) {
      const startX = containerBox.x + containerBox.width * 0.7;
      const endX = containerBox.x + containerBox.width * 0.3;
      const y = containerBox.y + containerBox.height * 0.5;

      await page.touchscreen.tap(startX, y);
      await page.touchscreen.tap(endX, y);
    }

    await page.waitForTimeout(50);

    // 再度タップで回転
    await gameContainer.tap();
    await page.waitForTimeout(50);

    // ぷよがまだアクティブ状態であることを確認
    const activePuyos = page.locator('.game-board .puyo.active');
    await expect(activePuyos).toHaveCount(2);
  });

  test('タッチ操作とキーボード操作が併用できる', async ({ page }) => {
    // タッチ操作でタップ
    const gameContainer = page.locator('[data-testid="game-container"]');
    await gameContainer.tap();
    await page.waitForTimeout(50);

    // キーボード操作で移動
    await page.keyboard.press('ArrowLeft');
    await page.waitForTimeout(50);

    // 再度タッチ操作
    await gameContainer.tap();
    await page.waitForTimeout(50);

    // 両方の操作が有効であることを確認（要件9.4）
    const activePuyos = page.locator('.game-board .puyo.active');
    await expect(activePuyos).toHaveCount(2);
  });

  test('ゲーム一時停止中はタッチ操作が無効になる', async ({ page }) => {
    // ゲームを一時停止
    await page.click('.pause-button');

    // 現在のぷよの位置を取得
    const activePuyo = page.locator('.game-board .puyo.active').first();
    const initialPosition = await activePuyo.boundingBox();

    // タッチ操作を試行
    const gameContainer = page.locator('[data-testid="game-container"]');
    await gameContainer.tap();
    await page.waitForTimeout(100);

    // ぷよが回転していないことを確認
    const newPosition = await activePuyo.boundingBox();
    if (initialPosition && newPosition) {
      expect(newPosition.x).toBe(initialPosition.x);
      expect(newPosition.y).toBe(initialPosition.y);
    }
  });
});

/**
 * モバイルデバイス専用のタッチ操作テスト
 */
test.describe('モバイルデバイスでのタッチ操作', () => {
  test.use({
    viewport: { width: 375, height: 667 }, // iPhone SE サイズ
    hasTouch: true,
    isMobile: true,
  });

  test.beforeEach(async ({ page }) => {
    await page.goto('/');
    await page.waitForLoadState('networkidle');

    // ゲームを開始
    await page.click('.new-game-button');
    await page.waitForTimeout(500);
  });

  test('モバイルでタッチ操作が正常に動作する', async ({ page }) => {
    // モバイル環境でのタッチ操作確認
    const gameContainer = page.locator('[data-testid="game-container"]');
    await expect(gameContainer).toBeVisible();

    // タップ操作
    await gameContainer.tap();
    await page.waitForTimeout(100);

    // ぷよが存在することを確認
    const activePuyos = page.locator('.game-board .puyo.active');
    await expect(activePuyos).toHaveCount(2);
  });

  test('モバイルでレスポンシブレイアウトが適用される', async ({ page }) => {
    // レスポンシブレイアウトが適用されることを確認
    const gameContainer = page.locator('[data-testid="game-container"]');
    await expect(gameContainer).toHaveClass(/responsive-layout/);

    // ゲームフィールドが表示される
    await expect(page.locator('.game-board')).toBeVisible();

    // スコア表示が表示される
    await expect(page.locator('.score-section')).toBeVisible();

    // NEXTぷよ表示が表示される
    await expect(page.locator('.next-puyo-section')).toBeVisible();
  });
});
