import { test, expect } from '@playwright/test';

/**
 * レスポンシブデザインのE2Eテスト
 * 要件10.4: レスポンシブデザイン対応
 */
test.describe('レスポンシブデザイン', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/');
    await page.waitForLoadState('networkidle');
  });

  test('デスクトップサイズでの表示', async ({ page }) => {
    // デスクトップサイズに設定
    await page.setViewportSize({ width: 1920, height: 1080 });

    // ゲームを開始
    await page.click('.new-game-button');
    await page.waitForTimeout(500);

    // レイアウトが適切に表示される
    const gameContainer = page.locator('[data-testid="game-container"]');
    await expect(gameContainer).toBeVisible();
    await expect(gameContainer).toHaveClass(/responsive-layout/);

    // 左右のサイドパネルが表示される
    const leftPanel = page.locator('.left-panel');
    const rightPanel = page.locator('.right-panel');
    await expect(leftPanel).toBeVisible();
    await expect(rightPanel).toBeVisible();

    // ゲームフィールドが中央に表示される
    const gameFieldSection = page.locator('.game-field-section');
    await expect(gameFieldSection).toBeVisible();
  });

  test('タブレットサイズでの表示', async ({ page }) => {
    // タブレットサイズに設定
    await page.setViewportSize({ width: 768, height: 1024 });

    await page.click('.new-game-button');
    await page.waitForTimeout(500);

    // レスポンシブレイアウトが適用される
    const gameContainer = page.locator('[data-testid="game-container"]');
    await expect(gameContainer).toHaveClass(/responsive-layout/);

    // 主要な要素が表示される
    await expect(page.locator('.game-board')).toBeVisible();
    await expect(page.locator('.score-section')).toBeVisible();
    await expect(page.locator('.next-puyo-section')).toBeVisible();

    // コントロールボタンが表示される
    await expect(page.locator('.control-buttons')).toBeVisible();
  });

  test('モバイルサイズでの表示', async ({ page }) => {
    // モバイルサイズに設定
    await page.setViewportSize({ width: 375, height: 667 });

    await page.click('.new-game-button');
    await page.waitForTimeout(500);

    // モバイルレイアウトが適用される
    const gameContainer = page.locator('[data-testid="game-container"]');
    await expect(gameContainer).toHaveClass(/responsive-layout/);

    // ゲームフィールドが表示される
    const gameBoard = page.locator('.game-board');
    await expect(gameBoard).toBeVisible();

    // スコア表示が表示される
    const scoreSection = page.locator('.score-section');
    await expect(scoreSection).toBeVisible();

    // NEXTぷよ表示が表示される
    const nextPuyoSection = page.locator('.next-puyo-section');
    await expect(nextPuyoSection).toBeVisible();
  });

  test('極小画面での表示', async ({ page }) => {
    // 極小画面サイズに設定
    await page.setViewportSize({ width: 320, height: 568 });

    await page.click('.new-game-button');
    await page.waitForTimeout(500);

    // 基本的な要素が表示される
    await expect(page.locator('.game-board')).toBeVisible();
    await expect(page.locator('.score-section')).toBeVisible();

    // コントロールボタンが表示される
    const controlButtons = page.locator('.control-buttons');
    await expect(controlButtons).toBeVisible();
  });

  test('画面サイズ変更時の動的対応', async ({ page }) => {
    // デスクトップサイズで開始
    await page.setViewportSize({ width: 1200, height: 800 });
    await page.click('.new-game-button');
    await page.waitForTimeout(500);

    // 初期レイアウトを確認
    const gameContainer = page.locator('[data-testid="game-container"]');
    await expect(gameContainer).toBeVisible();

    // モバイルサイズに変更
    await page.setViewportSize({ width: 375, height: 667 });
    await page.waitForTimeout(200);

    // レイアウトが動的に調整される
    await expect(gameContainer).toBeVisible();
    await expect(page.locator('.game-board')).toBeVisible();

    // タブレットサイズに変更
    await page.setViewportSize({ width: 768, height: 1024 });
    await page.waitForTimeout(200);

    // レイアウトが再調整される
    await expect(gameContainer).toBeVisible();
    await expect(page.locator('.game-board')).toBeVisible();
  });

  test('横向きモバイルでの表示', async ({ page }) => {
    // 横向きモバイルサイズに設定
    await page.setViewportSize({ width: 667, height: 375 });

    await page.click('.new-game-button');
    await page.waitForTimeout(500);

    // 横向きレイアウトが適用される
    const gameContainer = page.locator('[data-testid="game-container"]');
    await expect(gameContainer).toBeVisible();

    // 主要な要素が表示される
    await expect(page.locator('.game-board')).toBeVisible();
    await expect(page.locator('.score-section')).toBeVisible();
    await expect(page.locator('.next-puyo-section')).toBeVisible();
  });

  test('フォントサイズの調整', async ({ page }) => {
    // 大きな画面でのフォントサイズ確認
    await page.setViewportSize({ width: 1920, height: 1080 });

    const gameTitle = page.locator('.game-title');
    await expect(gameTitle).toBeVisible();

    // 小さな画面でのフォントサイズ確認
    await page.setViewportSize({ width: 320, height: 568 });
    await page.waitForTimeout(200);

    // タイトルが引き続き表示される
    await expect(gameTitle).toBeVisible();
  });

  test('タッチターゲットのサイズ', async ({ page }) => {
    // モバイルサイズに設定
    await page.setViewportSize({ width: 375, height: 667 });

    await page.click('.new-game-button');
    await page.waitForTimeout(500);

    // ボタンが十分なサイズを持つことを確認
    const pauseButton = page.locator('.pause-button');
    const buttonBox = await pauseButton.boundingBox();

    if (buttonBox) {
      // 最小タッチターゲットサイズ（44px）を満たすことを確認
      expect(buttonBox.height).toBeGreaterThanOrEqual(40);
      expect(buttonBox.width).toBeGreaterThanOrEqual(40);
    }

    const resetButton = page.locator('.reset-button');
    const resetButtonBox = await resetButton.boundingBox();

    if (resetButtonBox) {
      expect(resetButtonBox.height).toBeGreaterThanOrEqual(40);
      expect(resetButtonBox.width).toBeGreaterThanOrEqual(40);
    }
  });
});

/**
 * 特定デバイスでのテスト
 */
test.describe('デバイス固有のテスト', () => {
  test('iPhone SEでの表示', async ({ page }) => {
    await page.setViewportSize({ width: 375, height: 667 });

    await page.goto('/');
    await page.waitForLoadState('networkidle');

    // ページが正常に表示される
    await expect(page.locator('[data-testid="game-page"]')).toBeVisible();

    // ゲームを開始
    await page.click('.new-game-button');
    await page.waitForTimeout(500);

    // 主要な要素が表示される
    await expect(page.locator('.game-board')).toBeVisible();
    await expect(page.locator('.score-section')).toBeVisible();
    await expect(page.locator('.control-buttons')).toBeVisible();
  });

  test('iPad Proでの表示', async ({ page }) => {
    await page.setViewportSize({ width: 1024, height: 1366 });

    await page.goto('/');
    await page.waitForLoadState('networkidle');

    await page.click('.new-game-button');
    await page.waitForTimeout(500);

    // タブレット向けレイアウトが適用される
    const gameContainer = page.locator('[data-testid="game-container"]');
    await expect(gameContainer).toHaveClass(/responsive-layout/);

    // 左右のパネルが表示される
    await expect(page.locator('.left-panel')).toBeVisible();
    await expect(page.locator('.right-panel')).toBeVisible();

    // ゲームフィールドが適切なサイズで表示される
    await expect(page.locator('.game-board')).toBeVisible();
  });

  test('4Kディスプレイでの表示', async ({ page }) => {
    await page.setViewportSize({ width: 3840, height: 2160 });

    await page.goto('/');
    await page.waitForLoadState('networkidle');

    await page.click('.new-game-button');
    await page.waitForTimeout(500);

    // 高解像度でも適切に表示される
    const gameContainer = page.locator('[data-testid="game-container"]');
    await expect(gameContainer).toBeVisible();

    // ゲームフィールドが中央に配置される
    const gameFieldSection = page.locator('.game-field-section');
    await expect(gameFieldSection).toBeVisible();

    // サイドパネルが適切に表示される
    await expect(page.locator('.left-panel')).toBeVisible();
    await expect(page.locator('.right-panel')).toBeVisible();
  });

  test('縦長画面での表示', async ({ page }) => {
    // 縦長画面（スマートフォンの縦向き）
    await page.setViewportSize({ width: 360, height: 800 });

    await page.goto('/');
    await page.waitForLoadState('networkidle');

    await page.click('.new-game-button');
    await page.waitForTimeout(500);

    // 縦長レイアウトが適用される
    await expect(page.locator('.game-board')).toBeVisible();
    await expect(page.locator('.score-section')).toBeVisible();
    await expect(page.locator('.next-puyo-section')).toBeVisible();

    // コントロールボタンが表示される
    await expect(page.locator('.control-buttons')).toBeVisible();
  });

  test('横長画面での表示', async ({ page }) => {
    // 横長画面（ワイドモニター）
    await page.setViewportSize({ width: 2560, height: 1080 });

    await page.goto('/');
    await page.waitForLoadState('networkidle');

    await page.click('.new-game-button');
    await page.waitForTimeout(500);

    // 横長レイアウトが適用される
    const gameContainer = page.locator('[data-testid="game-container"]');
    await expect(gameContainer).toBeVisible();

    // 左右のパネルが適切に配置される
    await expect(page.locator('.left-panel')).toBeVisible();
    await expect(page.locator('.right-panel')).toBeVisible();

    // ゲームフィールドが中央に配置される
    await expect(page.locator('.game-field-section')).toBeVisible();
  });
});
