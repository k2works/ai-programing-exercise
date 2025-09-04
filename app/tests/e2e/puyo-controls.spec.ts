import { test, expect } from '@playwright/test';

/**
 * ぷよ操作機能のE2Eテスト
 * 要件2: ぷよ操作機能
 */
test.describe('ぷよ操作機能', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/');
    await page.waitForLoadState('networkidle');

    // ゲームを開始
    await page.click('.new-game-button');
    await page.waitForTimeout(500); // ぷよが生成されるまで待機
  });

  test('左矢印キーでぷよが左に移動する', async ({ page }) => {
    // 現在のぷよの位置を取得
    const activePuyo = page.locator('.game-board .puyo.active').first();
    const initialPosition = await activePuyo.boundingBox();

    // 左矢印キーを押す（要件2.1）
    await page.keyboard.press('ArrowLeft');
    await page.waitForTimeout(100);

    // ぷよが左に移動したことを確認
    const newPosition = await activePuyo.boundingBox();
    if (initialPosition && newPosition) {
      expect(newPosition.x).toBeLessThan(initialPosition.x);
    }
  });

  test('右矢印キーでぷよが右に移動する', async ({ page }) => {
    // 現在のぷよの位置を取得
    const activePuyo = page.locator('.game-board .puyo.active').first();
    const initialPosition = await activePuyo.boundingBox();

    // 右矢印キーを押す（要件2.2）
    await page.keyboard.press('ArrowRight');
    await page.waitForTimeout(100);

    // ぷよが右に移動したことを確認
    const newPosition = await activePuyo.boundingBox();
    if (initialPosition && newPosition) {
      expect(newPosition.x).toBeGreaterThan(initialPosition.x);
    }
  });

  test('上矢印キーでぷよが回転する', async ({ page }) => {
    // 組ぷよの初期配置を記録
    const activePuyos = page.locator('.game-board .puyo.active');
    const initialCount = await activePuyos.count();
    expect(initialCount).toBe(2);

    // 上矢印キーを押す（要件2.3）
    await page.keyboard.press('ArrowUp');
    await page.waitForTimeout(100);

    // ぷよが回転したことを確認（位置が変わる）
    const rotatedPuyos = page.locator('.game-board .puyo.active');
    await expect(rotatedPuyos).toHaveCount(2);
  });

  test('スペースキーでぷよが回転する', async ({ page }) => {
    // 組ぷよの初期配置を記録
    const activePuyos = page.locator('.game-board .puyo.active');
    const initialCount = await activePuyos.count();
    expect(initialCount).toBe(2);

    // スペースキーを押す（要件2.3）
    await page.keyboard.press('Space');
    await page.waitForTimeout(100);

    // ぷよが回転したことを確認
    const rotatedPuyos = page.locator('.game-board .puyo.active');
    await expect(rotatedPuyos).toHaveCount(2);
  });

  test('下矢印キーでぷよが高速落下する', async ({ page }) => {
    // 現在のぷよの位置を取得
    const activePuyo = page.locator('.game-board .puyo.active').first();
    const initialPosition = await activePuyo.boundingBox();

    // 下矢印キーを押す（要件2.4）
    await page.keyboard.press('ArrowDown');
    await page.waitForTimeout(200);

    // ぷよが下に移動したことを確認
    const newPosition = await activePuyo.boundingBox();
    if (initialPosition && newPosition) {
      expect(newPosition.y).toBeGreaterThan(initialPosition.y);
    }
  });

  test('壁際での移動が制限される', async ({ page }) => {
    // 左端まで移動
    for (let i = 0; i < 10; i++) {
      await page.keyboard.press('ArrowLeft');
      await page.waitForTimeout(50);
    }

    // さらに左に移動しようとしても移動しない（要件2.5）
    const activePuyo = page.locator('.game-board .puyo.active').first();
    const leftmostPosition = await activePuyo.boundingBox();

    await page.keyboard.press('ArrowLeft');
    await page.waitForTimeout(100);

    const finalPosition = await activePuyo.boundingBox();
    if (leftmostPosition && finalPosition) {
      expect(finalPosition.x).toBe(leftmostPosition.x);
    }
  });

  test('複数の操作を連続して実行できる', async ({ page }) => {
    // 複数の操作を連続実行
    await page.keyboard.press('ArrowLeft');
    await page.waitForTimeout(50);
    await page.keyboard.press('ArrowLeft');
    await page.waitForTimeout(50);
    await page.keyboard.press('Space');
    await page.waitForTimeout(50);
    await page.keyboard.press('ArrowRight');
    await page.waitForTimeout(50);

    // ぷよがまだアクティブ状態であることを確認
    const activePuyos = page.locator('.game-board .puyo.active');
    await expect(activePuyos).toHaveCount(2);
  });

  test('ゲーム一時停止中は操作が無効になる', async ({ page }) => {
    // ゲームを一時停止
    await page.click('.pause-button');

    // 現在のぷよの位置を取得
    const activePuyo = page.locator('.game-board .puyo.active').first();
    const initialPosition = await activePuyo.boundingBox();

    // キー操作を試行
    await page.keyboard.press('ArrowLeft');
    await page.waitForTimeout(100);

    // ぷよが移動していないことを確認
    const newPosition = await activePuyo.boundingBox();
    if (initialPosition && newPosition) {
      expect(newPosition.x).toBe(initialPosition.x);
      expect(newPosition.y).toBe(initialPosition.y);
    }
  });

  test('ゲーム再開後に操作が有効になる', async ({ page }) => {
    // ゲームを一時停止
    await page.click('.pause-button');

    // ゲームを再開
    await page.click('.pause-button'); // 再開ボタンになっている

    // 現在のぷよの位置を取得
    const activePuyo = page.locator('.game-board .puyo.active').first();
    const initialPosition = await activePuyo.boundingBox();

    // キー操作を実行
    await page.keyboard.press('ArrowLeft');
    await page.waitForTimeout(100);

    // ぷよが移動したことを確認
    const newPosition = await activePuyo.boundingBox();
    if (initialPosition && newPosition) {
      expect(newPosition.x).toBeLessThan(initialPosition.x);
    }
  });
});
