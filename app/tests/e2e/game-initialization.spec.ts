import { test, expect } from '@playwright/test';

/**
 * ゲーム初期化機能のE2Eテスト
 * 要件1: ゲーム開始機能
 */
test.describe('ゲーム初期化機能', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/');
    // ページが完全に読み込まれるまで待機
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
    await expect(page.locator('.game-board')).toBeVisible();

    // スコア表示が0で初期化される
    const scoreDisplay = page.locator('.score-section');
    await expect(scoreDisplay).toBeVisible();
    await expect(scoreDisplay).toContainText('0');

    // NEXTぷよ表示が表示される
    await expect(page.locator('.next-puyo-section')).toBeVisible();

    // 一時停止ボタンが表示される
    await expect(page.locator('.pause-button')).toBeVisible();

    // リセットボタンが表示される
    await expect(page.locator('.reset-button')).toBeVisible();
  });

  test('ゲームフィールドが正しいサイズで表示される', async ({ page }) => {
    await page.click('.new-game-button');

    // 12×6のゲームフィールドが表示される（要件10.1）
    const gameBoard = page.locator('.game-board');
    await expect(gameBoard).toBeVisible();

    // フィールドのセルが正しい数だけ存在する
    const cells = page.locator('[data-testid="game-field"] > div');
    await expect(cells).toHaveCount(72); // 12 × 6 = 72
  });

  test('初期状態でフィールドが空である', async ({ page }) => {
    await page.click('.new-game-button');

    // フィールドが空の状態で初期化される（要件1.2）
    const puyos = page.locator('[data-testid="game-field"] .puyo');
    await expect(puyos).toHaveCount(0);
  });

  test('最初の組ぷよが生成される', async ({ page }) => {
    await page.click('.new-game-button');

    // 少し待機してぷよが生成されるのを待つ
    await page.waitForTimeout(500);

    // 最初の組ぷよが表示される（要件1.4）
    const activePuyos = page.locator('[data-testid="game-field"] .puyo');
    await expect(activePuyos).toHaveCountGreaterThan(0); // ぷよが生成される
  });

  test('NEXTぷよが表示される', async ({ page }) => {
    await page.click('.new-game-button');

    // NEXTぷよが表示される（要件10.3）
    const nextPuyoDisplay = page.locator('.next-puyo-section');
    await expect(nextPuyoDisplay).toBeVisible();

    const nextPuyos = page.locator('.next-puyo-section .puyo');
    await expect(nextPuyos).toHaveCount(2); // NEXTぷよも2個
  });

  test('ゲームリセット機能が動作する', async ({ page }) => {
    await page.click('.new-game-button');

    // ゲームが開始されていることを確認
    await expect(page.locator('.pause-button')).toBeVisible();

    // リセットボタンをクリック
    await page.click('.reset-button');

    // スコアが0にリセットされる（要件1.3）
    const scoreDisplay = page.locator('.score-section');
    await expect(scoreDisplay).toContainText('0');

    // フィールドが空になる
    const puyos = page.locator('[data-testid="game-field"] .puyo');
    await expect(puyos).toHaveCount(0);

    // 新しいゲームボタンが再表示される
    await expect(page.locator('.new-game-button')).toBeVisible();
  });
});
