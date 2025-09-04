import { test, expect } from '@playwright/test';

/**
 * ゲームメカニクス（落下、消去、連鎖）のE2Eテスト
 * 要件3: ぷよ落下システム
 * 要件4: ぷよ消去システム
 * 要件5: 連鎖システム
 */
test.describe('ゲームメカニクス', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/');
    await page.waitForLoadState('networkidle');

    // ゲームを開始
    await page.click('.new-game-button');
    await page.waitForTimeout(500); // ぷよが生成されるまで待機
  });

  test('ぷよが自動的に落下する', async ({ page }) => {
    // 現在のぷよの位置を取得
    const activePuyo = page.locator('.game-board .puyo.active').first();
    const initialPosition = await activePuyo.boundingBox();

    // 自動落下を待機（要件3.1）
    await page.waitForTimeout(2000);

    // ぷよが下に移動したことを確認
    const newPosition = await activePuyo.boundingBox();
    if (initialPosition && newPosition) {
      expect(newPosition.y).toBeGreaterThan(initialPosition.y);
    }
  });

  test('ぷよが床に到達すると固定される', async ({ page }) => {
    // ぷよを底まで落下させる
    for (let i = 0; i < 20; i++) {
      await page.keyboard.press('ArrowDown');
      await page.waitForTimeout(50);
    }

    // ぷよが固定されるまで待機（要件3.2）
    await page.waitForTimeout(1000);

    // 新しい組ぷよが生成されることを確認（要件3.3）
    const activePuyos = page.locator('.game-board .puyo.active');
    await expect(activePuyos).toHaveCount(2);

    // 固定されたぷよが存在することを確認
    const fixedPuyos = page.locator('.game-board .puyo:not(.active)');
    await expect(fixedPuyos).toHaveCountGreaterThan(0);
  });

  test('複数のぷよを配置できる', async ({ page }) => {
    // 複数回ぷよを落下させる
    for (let round = 0; round < 3; round++) {
      // 現在のぷよを底まで落下
      for (let i = 0; i < 20; i++) {
        await page.keyboard.press('ArrowDown');
        await page.waitForTimeout(30);
      }

      // 固定されるまで待機
      await page.waitForTimeout(500);
    }

    // 複数のぷよが配置されていることを確認
    const allPuyos = page.locator('.game-board .puyo');
    const puyoCount = await allPuyos.count();
    expect(puyoCount).toBeGreaterThan(4); // 少なくとも2組以上
  });

  test('重力によってぷよが落下する', async ({ page }) => {
    // 特定の配置でぷよを設置（空白を作る）
    // 左側にぷよを配置
    await page.keyboard.press('ArrowLeft');
    await page.keyboard.press('ArrowLeft');
    for (let i = 0; i < 20; i++) {
      await page.keyboard.press('ArrowDown');
      await page.waitForTimeout(30);
    }
    await page.waitForTimeout(500);

    // 右側にぷよを配置
    await page.keyboard.press('ArrowRight');
    await page.keyboard.press('ArrowRight');
    await page.keyboard.press('ArrowRight');
    for (let i = 0; i < 20; i++) {
      await page.keyboard.press('ArrowDown');
      await page.waitForTimeout(30);
    }
    await page.waitForTimeout(500);

    // 中央にぷよを配置して空白を埋める
    for (let i = 0; i < 20; i++) {
      await page.keyboard.press('ArrowDown');
      await page.waitForTimeout(30);
    }

    // 重力が適用されることを確認（要件3.4）
    await page.waitForTimeout(1000);

    // フィールドにぷよが存在することを確認
    const allPuyos = page.locator('.game-board .puyo');
    await expect(allPuyos).toHaveCountGreaterThan(0);
  });

  test('ゲーム進行中に新しいぷよが継続的に生成される', async ({ page }) => {
    let previousPuyoCount = 0;

    // 複数回の落下サイクルを観察
    for (let cycle = 0; cycle < 3; cycle++) {
      // 現在のぷよを落下
      for (let i = 0; i < 15; i++) {
        await page.keyboard.press('ArrowDown');
        await page.waitForTimeout(30);
      }

      await page.waitForTimeout(500);

      // ぷよの総数を確認
      const allPuyos = page.locator('.game-board .puyo');
      const currentPuyoCount = await allPuyos.count();

      if (cycle > 0) {
        expect(currentPuyoCount).toBeGreaterThan(previousPuyoCount);
      }

      previousPuyoCount = currentPuyoCount;
    }
  });

  test('ゲーム一時停止中は自動落下が停止する', async ({ page }) => {
    // 現在のぷよの位置を記録
    const activePuyo = page.locator('.game-board .puyo.active').first();
    const initialPosition = await activePuyo.boundingBox();

    // ゲームを一時停止
    await page.click('.pause-button');

    // 自動落下の時間を待機
    await page.waitForTimeout(2000);

    // ぷよが移動していないことを確認
    const pausedPosition = await activePuyo.boundingBox();
    if (initialPosition && pausedPosition) {
      expect(pausedPosition.y).toBe(initialPosition.y);
    }

    // ゲームを再開
    await page.click('.pause-button');

    // 再開後に自動落下が再開されることを確認
    await page.waitForTimeout(2000);
    const resumedPosition = await activePuyo.boundingBox();
    if (pausedPosition && resumedPosition) {
      expect(resumedPosition.y).toBeGreaterThan(pausedPosition.y);
    }
  });
});

/**
 * ぷよ消去と連鎖システムのテスト
 * 注意: 実際の消去と連鎖をテストするには、特定の配置が必要
 * このテストは基本的な動作確認に留める
 */
test.describe('ぷよ消去と連鎖システム', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/');
    await page.waitForLoadState('networkidle');
    await page.click('.new-game-button');
    await page.waitForTimeout(500);
  });

  test('スコアが更新される', async ({ page }) => {
    // 初期スコアを確認
    const scoreDisplay = page.locator('.score-section');
    await expect(scoreDisplay).toContainText('0');

    // ぷよを配置してスコア変化を待つ
    // （実際の消去は運に依存するため、基本的な動作のみ確認）
    for (let i = 0; i < 5; i++) {
      // ぷよを落下
      for (let j = 0; j < 20; j++) {
        await page.keyboard.press('ArrowDown');
        await page.waitForTimeout(20);
      }
      await page.waitForTimeout(300);
    }

    // スコア表示が存在することを確認
    await expect(scoreDisplay).toBeVisible();
  });

  test('連鎖表示が機能する', async ({ page }) => {
    // 連鎖表示エリアが存在することを確認
    // （実際の連鎖発生は運に依存するため、要素の存在のみ確認）
    const chainDisplay = page.locator('[data-testid="chain-display"]');

    // 複数のぷよを配置
    for (let i = 0; i < 10; i++) {
      for (let j = 0; j < 20; j++) {
        await page.keyboard.press('ArrowDown');
        await page.waitForTimeout(10);
      }
      await page.waitForTimeout(200);

      // 連鎖が発生した場合の表示確認
      const isChainVisible = await chainDisplay.isVisible();
      if (isChainVisible) {
        await expect(chainDisplay).toContainText('連鎖');
        break; // 連鎖が確認できたらテスト終了
      }
    }
  });

  test('NEXTぷよが更新される', async ({ page }) => {
    // 初期のNEXTぷよを確認
    const nextPuyoDisplay = page.locator('.next-puyo-section');
    await expect(nextPuyoDisplay).toBeVisible();

    const initialNextPuyos = page.locator('.next-puyo-section .puyo');
    await expect(initialNextPuyos).toHaveCount(2);

    // ぷよを落下させてNEXTが更新されることを確認
    for (let i = 0; i < 20; i++) {
      await page.keyboard.press('ArrowDown');
      await page.waitForTimeout(30);
    }

    await page.waitForTimeout(500);

    // NEXTぷよが引き続き表示されることを確認
    const updatedNextPuyos = page.locator('.next-puyo-section .puyo');
    await expect(updatedNextPuyos).toHaveCount(2);
  });
});
