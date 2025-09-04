import { test, expect } from '@playwright/test';

/**
 * 基本機能のE2Eテスト
 * 実装済み機能の基本動作確認
 */
test.describe('基本機能テスト', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/');
    await page.waitForLoadState('networkidle');
  });

  test('アプリケーションが正常に表示される', async ({ page }) => {
    // ページタイトルを確認
    await expect(page).toHaveTitle(/ぷよぷよゲーム/);

    // メインゲームページが表示される
    await expect(page.locator('[data-testid="game-page"]')).toBeVisible();

    // ゲームタイトルが表示される
    await expect(page.locator('.game-title')).toHaveText('ぷよぷよゲーム');

    // 操作説明が表示される
    await expect(page.locator('.game-subtitle')).toContainText(
      '矢印キーで移動'
    );
  });

  test('新しいゲームボタンが表示される', async ({ page }) => {
    // 新しいゲームボタンが表示される
    const newGameButton = page.locator('.new-game-button');
    await expect(newGameButton).toBeVisible();
    await expect(newGameButton).toHaveText('新しいゲーム');
    await expect(newGameButton).toBeEnabled();
  });

  test('新しいゲームを開始できる', async ({ page }) => {
    // 新しいゲームボタンをクリック
    await page.click('.new-game-button');

    // ゲームフィールドが表示される
    await expect(page.locator('[data-testid="game-field"]')).toBeVisible();

    // スコア表示が0で初期化される
    const scoreDisplay = page.locator('.score-section');
    await expect(scoreDisplay).toBeVisible();
    await expect(scoreDisplay).toContainText('0');

    // NEXTぷよ表示が表示される
    await expect(page.locator('.next-puyo-section')).toBeVisible();

    // 一時停止ボタンが表示される
    await expect(page.locator('.pause-button')).toBeVisible();
  });

  test('ゲームフィールドが正しく表示される', async ({ page }) => {
    await page.click('.new-game-button');

    // 12×6のゲームフィールドが表示される
    const gameField = page.locator('[data-testid="game-field"]');
    await expect(gameField).toBeVisible();

    // フィールドのセルが正しい数だけ存在する
    const cells = page.locator('[data-testid="game-field"] > div');
    await expect(cells).toHaveCount(72); // 12 × 6 = 72
  });

  test('基本的なキーボード操作が機能する', async ({ page }) => {
    await page.click('.new-game-button');
    await page.waitForTimeout(500);

    // キーボード操作を実行
    await page.keyboard.press('ArrowLeft');
    await page.waitForTimeout(100);
    await page.keyboard.press('ArrowRight');
    await page.waitForTimeout(100);
    await page.keyboard.press('Space');
    await page.waitForTimeout(100);
    await page.keyboard.press('ArrowDown');
    await page.waitForTimeout(100);

    // ゲームが引き続き動作していることを確認
    await expect(page.locator('[data-testid="game-field"]')).toBeVisible();
    await expect(page.locator('.score-section')).toBeVisible();
  });

  test('ゲーム一時停止機能が動作する', async ({ page }) => {
    await page.click('.new-game-button');
    await page.waitForTimeout(500);

    // ゲームを一時停止
    await page.click('.pause-button');
    await expect(page.locator('.pause-button')).toContainText('再開');

    // ゲームを再開
    await page.click('.pause-button');
    await expect(page.locator('.pause-button')).toContainText('一時停止');
  });

  test('スコア表示が機能する', async ({ page }) => {
    await page.click('.new-game-button');

    // スコア表示が常に表示される
    const scoreDisplay = page.locator('.score-section');
    await expect(scoreDisplay).toBeVisible();

    // 初期スコアが0であることを確認
    await expect(scoreDisplay).toContainText('0');

    // スコア表示のテキストを取得
    const scoreText = await scoreDisplay.textContent();
    expect(scoreText).toMatch(/\d+/); // 数字が含まれることを確認
  });

  test('NEXTぷよ表示が機能する', async ({ page }) => {
    await page.click('.new-game-button');

    // NEXTぷよが表示される
    const nextPuyoDisplay = page.locator('.next-puyo-section');
    await expect(nextPuyoDisplay).toBeVisible();
  });

  test('レスポンシブレイアウトが適用される', async ({ page }) => {
    await page.click('.new-game-button');

    // レスポンシブレイアウトが適用される
    const gameContainer = page.locator('[data-testid="game-container"]');
    await expect(gameContainer).toBeVisible();
    await expect(gameContainer).toHaveClass(/responsive-layout/);
  });

  test('アクセシビリティ属性が設定されている', async ({ page }) => {
    // メインコンテンツにrole属性が設定されている
    const mainContent = page.locator('[data-testid="game-page"]');
    await expect(mainContent).toHaveAttribute('role', 'main');
    await expect(mainContent).toHaveAttribute('aria-label', 'ぷよぷよゲーム');

    // 新しいゲームボタンにaria-label属性が設定されている
    const newGameButton = page.locator('.new-game-button');
    await expect(newGameButton).toHaveAttribute(
      'aria-label',
      '新しいゲームを開始'
    );
  });
});

/**
 * クロスブラウザ基本テスト
 */
test.describe('クロスブラウザ基本テスト', () => {
  test('基本機能がブラウザで動作する', async ({ page, browserName }) => {
    console.log(`Testing on browser: ${browserName}`);

    await page.goto('/');
    await page.waitForLoadState('networkidle');

    // 基本的な表示確認
    await expect(page.locator('[data-testid="game-page"]')).toBeVisible();
    await expect(page.locator('.game-title')).toHaveText('ぷよぷよゲーム');

    // ゲーム開始
    await page.click('.new-game-button');
    await page.waitForTimeout(500);

    // 基本機能の確認
    await expect(page.locator('[data-testid="game-field"]')).toBeVisible();
    await expect(page.locator('.score-section')).toBeVisible();
    await expect(page.locator('.next-puyo-section')).toBeVisible();

    // 操作確認
    await page.keyboard.press('ArrowLeft');
    await page.keyboard.press('Space');
    await page.keyboard.press('ArrowDown');

    // ゲーム制御確認
    await page.click('.pause-button');
    await expect(page.locator('.pause-button')).toContainText('再開');

    await page.click('.pause-button');
    await expect(page.locator('.pause-button')).toContainText('一時停止');
  });
});
